# Copyright (c) 2026 Mister Fussy
# MIT License: Permission granted to use, copy, modify,
# and distribute this software. See LICENSE for details.
# Provided "AS IS" without warranty.

from PIL import Image

def text_to_image(input_file, output_file):
    # Read all lines and strip trailing newlines
    with open(input_file, "r") as f:
        lines = [line.rstrip("\n") for line in f]

    # Determine image size
    height = len(lines)
    width  = max(len(line) for line in lines)

    # Create an RGBA image (supports transparency)
    img    = Image.new("RGBA", (width, height), (0, 0, 0, 0))
    pixels = img.load()

    # Map characters to colors
    for y, line in enumerate(lines):
        for x, ch in enumerate(line):
            if   ch == "*":
                pixels[x, y] = (  0,   0,   0, 255)  # black
                pixels[x, y] = (  0, 255, 255, 255)  # cyan
            elif ch == "+":
                pixels[x, y] = (  0,   0, 255, 255)  # blue
                pixels[x, y] = (255, 255,   0, 255)  # yellow
            else:
                pixels[x, y] = (  0,   0,   0,   0)  # transparent
                               #  Red  Gren Blue Alpha

    img.save(output_file, "PNG")
    print(f"Saved image: {output_file}")

# Example usage:
# text_to_image("input.txt", "output.png")

text_to_image("led7segment.txt", "led7segment.png")
