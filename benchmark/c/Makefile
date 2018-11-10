APP = fib
CC = gcc
CFLAGS =

$(APP): $(APP).c time.c
	$(CC) -o $(APP) $^

$(APP)-m32: $(APP).c time.c
	$(CC) -m32 -o $(APP)-m32 $^

clean:
	$(RM) $(APP)
