import sys

header = """
\\documentclass[a4paper, margin=0]{article}
\\usepackage[left=5mm, right=0mm, top=5mm, bottom=0mm]{geometry}
\\usepackage{graphicx}
\\begin{document}
"""

footer = """
\\end{document}
"""

pagebreak = "\\newpage\n"

columnbreak = """
\\leavevmode
\\newline
"""


def image(img):
    return "\\noindent\\includegraphics[width=59mm]{" + img + "}\n"

def main(cards):
    output = ""
    output += header
    ic = 0

    for i in cards:
        ic += 1
        output += image(i)

        if ic == 9:
            ic = 0
            output += pagebreak

        if ic % 3 == 0:
            output += columnbreak

    output += footer

    print(output)

main(sys.argv[1::])
