import time

def get_millis():
    return int(round(time.time() * 1000))


def get_ellapsed_time(start, end):
    return (end - start) / float(1000)


def fib(n):
    if n < 2:
        return 1
    else:
        return fib(n - 1) + fib(n - 2)


def main():
    W = 10
    N = 30

    n = 40

    for i in range(W):
        print "WARMUP: %d" % (i)
        fib(n)
    start = time.time()
    for j in range(N):
        print "EXECUTION: %d" % (j)
        fib(n)
    end = time.time()
    print "TIME: %f" % (get_ellapsed_time(start, end))


if __name__ == "__main__":
    main()
