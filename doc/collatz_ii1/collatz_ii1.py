def collatz(n):
    cur0 = n
    step = 0
    if cur0 != 1:
        step = 1
        if cur0 % 2 == 0:
            cur1 = cur0 // 2
        else:
            cur1 = 3 * cur0 + 1
        while cur1 != 1:
            c1 = cur0 // 2 if cur0 % 2 == 0 else 3 * cur0 + 1
            c2 = c1 // 2 if c1 % 2 == 0 else 3 * c1 + 1
            cur0 = cur1
            cur1 = c2
            step += 1
        return step
    else:
        return step

def test():
    print(f"collatz(10) = {collatz(10)}")
    print(f"collatz(20) = {collatz(20)}")
    print(f"collatz(30) = {collatz(30)}")

if __name__ == '__main__':
    test()