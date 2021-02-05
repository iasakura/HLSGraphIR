def collatz(n):
    cur = n
    step = 0
    while cur != 1:
        print(f'{cur}, {step}')
        if cur % 2 == 0:
            cur = cur // 2
        else:
            cur = 3 * cur + 1
        step += 1
    return step

def test():
    print(f"collatz(10) = {collatz(10)}")
    print(f"collatz(20) = {collatz(20)}")
    print(f"collatz(30) = {collatz(30)}")

if __name__ == '__main__':
    test()