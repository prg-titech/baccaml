import sys
import time
import statistics as stc


def micros():
    return int(time.time() * 100000.0)


def summary(n):
    if n == 0:
        return 0
    else:
        return n + summary(n - 1)


def main():
    W = 0
    N = 100

    n = 6000

    res = []
    start = micros()
    for j in range(N):
        estart = micros()
        summary(n)
        eend = micros()
        res.append(eend - estart)
        print "%f" % (eend - estart)
    end = micros()

    ave = stc.mean(res)
    stdev = stc.stdev(res)
    print "TIME: %f us" % ((end - start) / float(N))
    print "Average: %f \t STDEV: %f" % (ave, stdev)



if __name__ == "__main__":
    sys.setrecursionlimit(6000)
    main()
