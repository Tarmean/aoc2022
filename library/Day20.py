class Node:
    def __init__(self, val):
        self.left = None
        self.right = None
        self.value = val
    def offset(self, off):
        new = self
        assert(off >= 0)
        for i in range(off):
            new = new.right
        return new
    def to_list(self):
        cur = self.right
        out = [self.value]
        while cur is not self:
            out.append(cur.value)
            cur = cur.right
        return out

class LinkedList:
    def __init__(self, source):
        self.nodes = [Node(e) for e in source]
        self.lookup = { n.value: n for n in self.nodes }

        self.nodes[0].left = self.nodes[-1]
        self.nodes[-1].right = self.nodes[0]
        for i in range(len(self.nodes)-1):
            self.nodes[i].right = self.nodes[i+1]
            self.nodes[i+1].left = self.nodes[i]

    def move_node(self, n):
        n.left.right = n.right
        n.right.left = n.left

        off = n.value % (len(self.nodes)-1)
        new = n.left.offset(off)
        newR = new.right

        new.right = n
        n.left = new
        newR.left = n
        n.right = newR
    def mixes(self):
        for n in self.nodes:
            self.move_node(n)
    def output(self):
        base = self.lookup[0]
        a = base.offset(1000)
        b = a.offset(1000)
        c = b.offset(1000)
        return a.value,b.value,c.value

inp = [1, 2, -3, 3, -2, 0, 4]

if __name__ == "__main__":
    with open("input/Day20.txt") as inp:
        inp = list(811589153 * int(n) for n in inp.read().splitlines())
        n = LinkedList(inp)
        for i in range(10):
            n.mixes()
        print(sum(n.output()))





