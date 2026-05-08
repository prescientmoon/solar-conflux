charmap = [
    ('a', 0b00100),
    ('e', 0b00111),
    ('i', 0b10100),
    ('u', 0b01101),
    ('s', 0b01101),
    ('t', 0b11110),
    ('m', 0b10110),
]

input="""
10010110
-
10001001
10001111
-
10011110
-
-
10000100
10010100
-
-
10000100
-
10001110
10011010
-
10011010
10010100
10001001
-
-
10010010
10011010
-
10000110
10001110
-
10010101
10001101
10000110
-
-
10000100
10001110
"""

input2 = """
10100
-
-
-
-
11110

-
-
-
00110
10101
-

-
00110
01001
10110
00100
11010

11010
10110
10101
11110
10110
-

-
00100
01001
-
-
-

01101
-
-
-
-
10010
"""

def shift_string(s: str) -> str:
    return s[-1] + s[:-1] if s else s

def reverse_string(s: str) -> str:
    return s[::-1]

def int_array_to_string(arr):
    return "".join(chr(i +97) if i is not None else ' ' for i in arr)

def parse_lines(data: str, shift = 0, flip = False):
    result = []
    for line in data.splitlines():
        line = line.strip()
        if not line:
            continue
        if line == "-":
            result.append(None)
            continue
        else:
            line= line[-5:]
            for _ in range(shift):
                line = shift_string(line)
            if flip:
                line = reverse_string(line)
            result.append(int(line, 2)-4)
    return result

# for flip in [0]:
#     for shift in range(1):
p1 = parse_lines(input, flip=True)
# print(p1)
print(int_array_to_string(p1))
p2 = parse_lines(input2, flip=True)
# print(p2)
print(int_array_to_string(p2))
print(int_array_to_string([26]))

# def caesar_cipher_range(s, shift_range):
#     results = []
#     for shift in shift_range:
#         encrypted = "".join(
#             chr((ord(c) - 97 + shift) % 26 + 97) if c.islower() else
#             chr((ord(c) - 65 + shift) % 26 + 65) if c.isupper() else c
#             for c in s
#         )
#         results.append((shift, encrypted))
#     return results
#
#
# shifts = range(0, 26)
# text = int_array_to_string(parsed_data)
# for shift, result in caesar_cipher_range(text, shifts):
#     print(f"Shift {shift}: {result}")
