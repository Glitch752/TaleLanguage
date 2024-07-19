//! Reference-counted Shared Pointer
//! From: https://github.com/yrashk/zig-rcsp/tree/master
//!
//! Supports both atomic and non-atomic counters.
//!
//! Here's an example of its usage:
//!
//! ```
//! const AtomicU128Counter = RcSharedPointer(u128, Atomic);
//! var counter = AtomicU128Counter(100, std.heap.page_allocator);
//! _ = defer counter.deinit();
//! var counter1 = counter.strongClone();
//! _ = counter1.ptr();
//! _ = defer counter1.deinit();
//! ```

const std = @import("std");
const builtin = @import("builtin");
const testing = std.testing;

/// Atomic counter
///
/// When used with single threaded builds, will defer to `NonAtomic`
pub const Atomic = if (builtin.single_threaded) NonAtomic else struct {
    const T = usize;
    pub const MAX = std.math.maxInt(T);
    pub const MIN = std.math.minInt(T);

    /// Saturating increment
    inline fn increment(ptr: *T) T {
        var val = @atomicLoad(T, ptr, .Monotonic);
        while (@cmpxchgWeak(T, ptr, val, if (val == MAX) val else val + 1, .Monotonic, .Monotonic)) |v| {
            val = v;
        }
        return val;
    }

    /// Bottom-clamped saturating increment
    /// (if counter is zero, it will not be incremented)
    inline fn clampedIncrement(ptr: *T) T {
        var val = @atomicLoad(T, ptr, .Monotonic);
        while (@cmpxchgWeak(T, ptr, val, if (val == MAX or val == MIN) val else val + 1, .Monotonic, .Monotonic)) |v| {
            val = v;
        }
        return val;
    }

    /// Saturating decrement
    inline fn decrement(ptr: *T) T {
        var val = @atomicLoad(T, ptr, .Acquire);
        while (@cmpxchgWeak(T, ptr, val, if (val == MIN) val else val - 1, .Release, .Monotonic)) |v| {
            val = v;
        }
        return val;
    }

    /// Load counter value
    inline fn load(ptr: *T) T {
        return @atomicLoad(T, ptr, .SeqCst);
    }

    /// Establish ordering with the counters
    inline fn synchronize() void {
        @fence(.Acquire);
    }
};

/// Non-atomic counter
pub const NonAtomic = struct {
    const T = usize;
    pub const MAX = std.math.maxInt(T);
    pub const MIN = std.math.minInt(T);

    /// Saturating increment
    inline fn increment(ptr: *T) T {
        const val = ptr.*;
        const res = @addWithOverflow(val, 1);
        if (res[1] == 1) {
            ptr.* = MAX;
        } else {
            ptr.* = res[0];
        }
        return val;
    }
    /// Bottom-clamped saturating increment
    /// (if counter is zero, it will not be incremented)
    inline fn clampedIncrement(ptr: *T) T {
        const val = ptr.*;
        if (val == MIN) {
            return MIN;
        }
        const res = @addWithOverflow(val, 1);
        if (res[1] == 1) {
            ptr.* = MAX;
        } else {
            ptr.* = res[0];
        }
        return val;
    }

    /// Saturating decrement
    inline fn decrement(ptr: *T) T {
        const val = ptr.*;
        const res = @subWithOverflow(val, 1);
        if (res[1] == 1) {
            ptr.* = MIN;
        } else {
            ptr.* = res[0];
        }
        return val;
    }

    /// Load counter value
    inline fn load(ptr: *T) T {
        return ptr.*;
    }

    /// Establish ordering with the counters
    inline fn synchronize() void {}
};

/// Reference-counted shared pointer
///
/// Shared pointer with `Atomic` operations should not use
/// the same clone in more than one thread simultaneously.
///
/// Shared pointer with `NonAtomic` operations should not use
/// any clones outside of a single thread simultaneously.
pub fn RcSharedPointer(comptime T: type, comptime Ops: type) type {
    _ = T.deinit; // T must have a deinit method

    const Inner = struct {
        val: T,
        strong_ctr: usize = 1,
        weak_ctr: usize = 1,
        allocator: std.mem.Allocator,
    };
    return struct {
        const Strong = @This();
        pub const Weak = struct {
            inner: ?*Inner,
            pub const Type = T;

            // There's seemingly a bug in Zig that messes with
            // creation of RcSharedPointer if the constant below
            // is declared as `Self` (and is later reused in the
            // outer scope)
            // TODO: change this to `Self` when (if) this behaviour
            // will be changed
            const SelfWeak = @This();

            /// Create a strong clone
            ///
            /// Might return zero if no other strong clones are present
            /// (which indicates that the value has been deinitialized,
            /// but not deallocated)
            ///
            /// Instead of upgrading a shared pointer or its
            /// strong clone to a weak one, creation of a weak
            /// clone is used to avoid any potential race conditions
            /// caused by momentarily inconsistent strong and weak
            /// counters (where the total number of counters might
            /// be incorrect during downgrade or upgrade operations)
            pub fn strongClone(self: SelfWeak) ?Strong {
                // the reason we're doing a clamped increment here is
                // because if the counter is already zero, then the value
                // has been deinitialized,..
                const prev = Ops.clampedIncrement(&self.inner.?.*.strong_ctr);
                if (prev == Ops.MAX) {
                    @panic("strong counter has been saturated");
                }
                if (prev == Ops.MIN) {
                    // ..so, we'll not be able to make a strong clone anymore
                    return null;
                }
                return Strong{ .inner = self.inner };
            }

            /// Create a weak clone
            pub fn weakClone(self: SelfWeak) SelfWeak {
                const prev = Ops.increment(&self.inner.?.*.weak_ctr);
                if (prev == Ops.MAX) {
                    @panic("weak counter has been saturated");
                }
                return SelfWeak{ .inner = self.inner };
            }

            /// Number of strong clones
            pub inline fn strongCount(self: SelfWeak) usize {
                return Ops.load(&self.inner.?.*.strong_ctr);
            }

            /// Number of weak clones
            pub inline fn weakCount(self: SelfWeak) usize {
                return Ops.load(&self.inner.?.*.weak_ctr) - 1;
            }

            /// Deinitialize weak clone
            ///
            /// Will never deinitialize the value but will
            /// deallocate it if it is the last clone (both strong and weak)
            ///
            /// Returns true if the value was deallocated
            pub fn deinit(self: *SelfWeak) bool {
                const cw_ = Ops.decrement(&self.inner.?.*.weak_ctr);
                const p = self.inner.?;
                // incapacitate self (useful methods will now panic)
                self.inner = null;
                // if weak counter was not saturated
                if (cw_ == 1) {
                    Ops.synchronize();
                    // then we can deallocate
                    p.*.allocator.destroy(p);
                    return true;
                }
                return false;
            }
        };

        inner: ?*Inner,
        pub const Type = T;

        const Self = @This();

        /// Initialize the counter with a value
        ///
        /// Allocates memory to hold the value and the counter
        pub fn init(val: T, allocator: std.mem.Allocator) !Self {
            const allocated = try allocator.create(Inner);
            allocated.* = Inner{
                .val = val,
                .allocator = allocator,
            };
            return Self{ .inner = allocated };
        }

        /// Create a strong clone
        pub fn strongClone(self: Self) Self {
            // the reason we're not doing a clampedIncrement here (as we do in `Weak`)
            // is that the presence of non-null `self.inner` is already a guarantee that
            // there's at least one strong clone present (`self`)
            const prev = Ops.increment(&self.inner.?.*.strong_ctr);
            if (prev == Ops.MAX) {
                @panic("strong counter has been saturated");
            }
            return Self{ .inner = self.inner };
        }

        /// Create a weak clone
        ///
        /// Instead of downgrading a shared pointer or its
        /// strong clone to a weak one, creation of a weak
        /// clone is used to avoid any potential race conditions
        /// caused by momentarily inconsistent strong and weak
        /// counters (where the total number of counters might
        /// be incorrect during downgrade or upgrade operations)
        pub fn weakClone(self: Self) Weak {
            const prev = Ops.increment(&self.inner.?.*.weak_ctr);
            if (prev == Ops.MAX) {
                @panic("weak counter has been saturated");
            }
            return Weak{ .inner = self.inner };
        }

        /// Number of strong clones
        pub inline fn strongCount(self: Self) usize {
            return Ops.load(&self.inner.?.*.strong_ctr);
        }

        /// Number of weak clones
        pub inline fn weakCount(self: Self) usize {
            return Ops.load(&self.inner.?.*.weak_ctr) - 1;
        }

        /// Const pointer to the value
        ///
        /// As the pointer is constant, if mutability
        /// is desired, use of `std.Mutex` and `unsafePtr`
        /// is recommended
        pub fn ptr(self: Self) *const T {
            return &self.inner.?.*.val;
        }

        /// Unsafe (mutable) pointer to the value
        /// Normally it is recommended to use `std.Mutex`
        /// for concurrent access:
        ///
        /// ```
        /// const T = struct { value: u128, ptr: std.Mutex = std.Mutex.init() };
        /// var counter = RcSharedPointer(T, Atomic).init(T{ .value = 10 });
        /// defer counter.deinit();
        /// var ptr = counter.unsafePtr();
        /// {
        ///     const lock = ptr.*.mutex.aquire();
        ///     defer lock.release();
        ///     ptr.*.value = 100;
        /// }
        /// ```
        pub fn unsafePtr(self: Self) *T {
            return &self.inner.?.*.val;
        }

        /// Deinitialize the shared pointer
        ///
        /// Will deallocate its initial allocation
        ///
        /// Return true if the value was deallocated
        pub fn deinit(self: *Self) bool {
            return self.deinitWithCallback(?void, null, null);
        }

        /// Deinitialize the shared pointer with a callback
        ///
        /// Will first deinitialize the value using the callback
        /// (if there are no other strong clones present) and then
        /// deallocate its initial allocation (if there are no weak
        /// clones present)
        ///
        /// Return true if the value was deallocated
        pub fn deinitWithCallback(self: *Self, comptime C: type, context: C, deinitializer: ?fn (*T, C) void) bool {
            const c_ = Ops.decrement(&self.inner.?.*.strong_ctr);
            Ops.synchronize();
            const p = self.inner.?;
            // incapacitate self (useful methods will now panic)
            self.inner = null;
            if (c_ == 1) {
                // ...ready to deinitialize the value
                if (deinitializer) |deinit_fn| {
                    deinit_fn(&p.*.val, context);
                }
                const cw = Ops.decrement(&p.*.weak_ctr);
                // also, if there are no outstanding weak counters,
                if (cw == 1) {
                    Ops.synchronize();
                    // then deallocate
                    p.val.deinit(p.allocator);
                    p.allocator.destroy(p);
                    return true;
                }
            }
            return false;
        }
    };
}
