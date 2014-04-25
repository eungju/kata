def conjunction(a, b):
    result = []
    for c in map(lambda x: chr(x), range(ord("a"), ord("z") + 1)):
        minimum = min(a.count(c), b.count(c))
        if minimum > 0: result.append((c, minimum))
    return result

def candidates(l):
    if len(l) == 0:
        return [""]
    else:
        (c, n) = l[0]
        return [(c * x) + y for x in range(n, -1, -1) for y in candidates(l[1:])]

def permutation(l):
    if len(l) == 0:
        return [""]
    else:
        return [l[i] + y for i in range(0, len(l)) for y in permutation(l[0:i] + l[i+1:])]

def common_permutation(a, b):
    for c in candidates(conjunction(a, b)):
        for p in permutation(c):
            print p
            if a.find(p) >= 0 and b.find(p) >= 0:
                return c

assert "e" == common_permutation("pretty", "women")
assert "nw" == common_permutation("walking", "down")
assert "et" == common_permutation("the", "street")
atoz = "abcdefghijklmnopqrstuvwxyz"
n = 1
#assert atoz * n == common_permutation(atoz * n, atoz * n)
#print conjunction(atoz * 40, atoz * 40)
