def main():
	print(parent())

def add(x,y):
	return x + y

def mul(x,y):
	return x * y

def parent():
	a = 0
	x = 200
	y = 10
	z = True
	if z:
		a = add(x,y)
		a += 1
	return a

main()
