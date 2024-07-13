# Speeds up an asciinema recording (since you can't do that when recording in PowerSession).
# This probably doesn't need to be in the repository, but it's nice to have for other recordings I make for now.

def main():
    with open('.\donut.tale.rec', 'r') as f:
        lines = f.readlines()

    speed = 5

    for line in lines[1:]:
        if line.strip() == '':
            continue

        originalSpeed = line.split('[')[1].split(",")[0].strip()
        newSpeed = float(originalSpeed) / speed
        newLine = line.replace(originalSpeed, str(newSpeed))

        lines[lines.index(line)] = newLine

    # Write the new lines to a new file
    with open('.\donut.tale.speed.rec', 'w') as f:
        f.writelines(lines)

if __name__ == '__main__':
    main()