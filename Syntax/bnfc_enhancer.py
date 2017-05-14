#!/usr/bin/env python
import os
import re

def transformAbsGrammar():
    with open('Syntax/AbsGrammar.hs', 'r+') as f:
        text = f.read()
        types = re.findall(r"data ([a-zA-Z0-9]+)", text)
        for t in types:
            for _ in range(10):
                text = re.sub(r"(data\s+)" + t, r"\1PRESERVE", text)
                text = re.sub(r"([=\|]\s*)" + t, r"\1PRESERVE", text)
                text = re.sub(
                    r"(\s+|\[)" + t + r"(\s+|\])",
                    r"\1" + r"(Pos " + t + r")" + r"\2",
                    text,
                )
                text = re.sub("PRESERVE", t, text)
        types = re.findall(r"newtype ([a-zA-Z0-9]+)", text)
        text += """
class Positioned a where
    left_end :: a -> (Int, Int)
    right_end :: a -> (Int, Int)
        """.strip() + "\n\n"
        for t in types:
            text += """
instance Positioned %s where
    left_end (%s ((line, col), _)) = (line, col)
    right_end (%s ((line, col), tok)) = (line, col + length tok)
            """.strip() % (t, t, t) + "\n\n"
        text += """
data Pos a = Pos (Int, Int) (Int, Int) a
    deriving (Eq,Ord,Show)

instance Positioned (Pos a) where
    left_end (Pos l _ _) = l
    right_end (Pos _ r _) = r

instance Positioned a => Positioned [a] where
    left_end [] = (-1, -1)
    left_end li = left_end $ head li
    right_end [] = (-1, -1)
    right_end li = right_end $ last li
        """.strip() + "\n\n"
        #print text
        f.seek(0)
        f.write(text)
        f.truncate()

def transformParGrammar():
    with open('Syntax/ParGrammar.y', 'r+') as f:
        parser = f.read()
        (header, body) = parser.split('%%')
        body = body.split('\n')
        result = []
        for line in body:
            if 'mkPosToken' in line or (' :: ' in line and ' : L_' in line):
                pass
            elif ' :: { [' in line:
                line = line.replace(' :: { [', ' :: { [Pos ')
            elif ' :: {' in line:
                line = line.replace(' :: { ', ' :: { Pos ')
            elif ' : ' in line or '| ' in line:
                line = re.sub(r' \{ ([A-Z]+)', r' { Pos (left_end $1) (right_end $>) \$ \1', line)
            elif line == "}":
                line = """
instance Positioned Token where
    left_end = tokenLineCol
    right_end token = (line, col + length str) where
        (line, col) = tokenLineCol token
        str = prToken token
}
                """
            else:
                pass
            result.append(line)
        result = "\n".join(result)
        body = result
        parser = header + '\n%%\n' + body
        f.seek(0)
        f.write(parser)
        f.truncate()

def main():
    transformAbsGrammar()
    transformParGrammar()

if __name__ == "__main__":
    main()
