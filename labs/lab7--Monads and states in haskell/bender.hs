type Pos = (Int, Int)

start = (0,0)

up (x, y)    = (x, y+1)
down (x, y)  = (x, y-1)
left (x, y)  = (x-1, y)
right (x, y) = (x+1, y)

x -: f = f x

-- Using the "-:" operator, we can chain movements together
test1 = start -: up -: right
test2 = start -: up -: left -: left -: right -: down

