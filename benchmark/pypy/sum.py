import time

def summary(n):
    if n == 0: return 0
    else: return n + summary(n -1)

def main():
    W = 10
    N = 20

    n = 50

    res = 0

    wstart = time.time()
    for i in range(W):
        tstart = time.time()
        res = summary(n)
        print "Warmup (%d) elapsed time: %f s" % (i + 1, (time.time() - tstart))

    wend = time.time()

    print "WARMUP TIME: %f s" % ((wend - wstart) / float(W))
    start = time.time()
    for j in range(N):
        estart = time.time()
        res = summary(n)
        print "Execution (%d) elapsed time: %f s" % (j + 1, (time.time() - estart))

    end = time.time()
    print "TIME: %f s" % ((end - start) / float(N))


if __name__ == "__main__":
    main()
