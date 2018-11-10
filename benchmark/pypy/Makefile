APP = fib

$(APP): $(APP).py
	pypy $^

fib-nojit: fib.py
	pypy --jit off $^
