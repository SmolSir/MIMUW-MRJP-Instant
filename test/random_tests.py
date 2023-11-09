import sys
import random
from typing import Optional

random.seed(42)
sys.setrecursionlimit(1000000)


class Expr:
    def __init__(
            self, m: int, k: int, max_value: int = 20, ops: [str] = ["+", "-", "*", "/"]
    ):
        self.p = [0] * (2 * m + 1)
        self.r = [0] * (2 * m + 1)
        self.l = [0] * (2 * m + 1)
        deg0 = set([0])
        for i in range(m):
            p = random.choice(list(deg0)[-k:])
            self.p[2 * i + 1] = p
            self.p[2 * i + 2] = p
            self.l[p] = 2 * i + 1
            self.r[p] = 2 * i + 2
            deg0.remove(p)
            deg0.add(2 * i + 1)
            deg0.add(2 * i + 2)

        self.c = [
            random.choice(ops) if i not in deg0 else str(random.randint(0, max_value))
            for i in range(2 * m + 1)
        ]

    def __str__(self):
        def node_str(i: int):
            if self.l[i] == 0:
                return self.c[i]
            else:
                return f"({node_str(self.l[i])} {self.c[i]} {node_str(self.r[i])})"

        return node_str(0)

    def eval(self):
        def node_eval(i: int):
            if self.l[i] == 0:
                return int(self.c[i])
            else:
                l = node_eval(self.l[i])
                r = node_eval(self.r[i])
                if self.c[i] == "+":
                    return l + r
                elif self.c[i] == "-":
                    return l - r
                elif self.c[i] == "*":
                    return l * r
                elif self.c[i] == "/":
                    if (l >= 0) != (r >= 0) and l % r != 0:
                        return l // r + 1
                    else:
                        return l // r

        return node_eval(0)

    def stack_limit(self):
        def node_stack_limit(i: int):
            if self.l[i] == 0:
                return 1
            else:
                l = node_stack_limit(self.l[i])
                r = node_stack_limit(self.r[i])
                return min(max(l + 1, r), max(r + 1, l))

        return node_stack_limit(0)

    def save(self, filename: str):
        with open(filename + ".ins", "w") as file:
            file.write(str(self) + "\n")
        with open(filename + ".output", "w") as file:
            file.write(str(self.eval()) + "\n")


class ExprGroup:
    def __init__(
            self,
            count: int,
            m: int,
            k: Optional[int],
            max_value: int = 20,
            ops: [str] = ["+", "-", "*", "/"],
    ):
        self.count = count
        self.m = m
        self.k = k if k is not None else 2 * m
        self.max_value = max_value
        self.ops = ops

    def generate(self):
        all = []
        for _ in range(self.count):
            while True:
                e = Expr(self.m, self.k, self.max_value, self.ops)
                try:
                    e.eval()
                    break
                except ZeroDivisionError:
                    continue
            all += [e]
        return all


class ExprGroupSmall(ExprGroup):
    def __init__(self, count: int, m: int, k: Optional[int] = None):
        super().__init__(count, m, k, max_value=20, ops=["+", "-", "*", "/"])


class ExprGroupBig(ExprGroup):
    def __init__(self, count: int, m: int, k: Optional[int] = None):
        super().__init__(count, m, k, max_value=5, ops=["+", "-"])


groups = [
    ExprGroupSmall(count=10, m=5),
    ExprGroupSmall(count=5, m=5, k=2),
    ExprGroupSmall(count=1, m=5, k=1),
    ExprGroupSmall(count=10, m=20),
    ExprGroupSmall(count=5, m=20, k=5),
    ExprGroupSmall(count=1, m=20, k=1),
    ExprGroupBig(count=10, m=100),
    ExprGroupBig(count=5, m=100, k=10),
    ExprGroupBig(count=1, m=100, k=1),
    ExprGroupBig(count=20, m=1_000),
    ExprGroupBig(count=10, m=1_000, k=10),
    ExprGroupBig(count=1, m=1_000, k=1),
    ExprGroupBig(count=5, m=10_000),
    ExprGroupBig(count=1, m=10_000, k=2),
    ExprGroupBig(count=1, m=10_000, k=1),
]

all = []
for g in groups:
    all += g.generate()


def save_tests(filename_prefix: str):
    for i, e in enumerate(all):
        e.save(f"{filename_prefix}{(i + 1):02d}")
