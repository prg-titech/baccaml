import time

def fib(n):
    if n < 2:
        return 1
    else:
        return fib(n - 1) + fib(n - 2)


def main():
    W = 10
    N = 90

    n = 40

    res = 0

    wstart = time.time()
    for i in range(W):
        tstart = time.time()
        res = fib(n)
        print "%f," % ((time.time() - tstart))

    wend = time.time()

    print "WARMUP TIME: %f s" % ((wend - wstart) / float(W))
    start = time.time()
    for j in range(N):
        estart = time.time()
        res = fib(n)
        print "%f," % ((time.time() - estart))

    end = time.time()
    print "TIME: %f s" % ((end - start) / float(N))


if __name__ == "__main__":
    main()
