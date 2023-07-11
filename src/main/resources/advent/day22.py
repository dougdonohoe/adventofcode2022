# From https://anceps.net/advent/?d=2022_22
import re

RIGHT, DOWN, LEFT, UP = list(range(4))
DIRS = [(1,0), (0,1), (-1,0), (0,-1)]
SYMBOLS = ['>', 'v', '<', '^']


def readInput():
    with open("day22.txt", 'r', encoding='utf-8') as f:
        lines = [line[:-1] for line in f.readlines()]

    instructions = tuple(map(lambda x:x in 'RL' and x or int(x), re.findall('[0-9]+|[RL]', lines[-1])))
    lines = lines[:-2]

    height = len(lines)
    width = 0

    nbVisiblePoints = 0
    for line in lines:
        if len(line) > width:
            width = len(line)
        nbVisiblePoints += len(line) - line.count(' ')

    sideLength = (nbVisiblePoints / 6.)**0.5
    assert sideLength == int(sideLength)
    sideLength = int(sideLength)

    ### Add some margin to go from position on cube in any direction without out of index error
    lines = [width*" "] + lines + [width*" "]
    for i in range(len(lines)):
        lines[i] = " " + lines[i] + (width+1)*" "
    width += 2
    height += 2

    return lines, instructions, width, height, sideLength


lines, INSTRUCTIONS, WIDTH, HEIGHT, SIDE_LENGTH = readInput()

START_Y = 1
START_X = lines[START_Y].index('.')


# Part 1:  gives the next position, None if obstacle
def nextPoint_part1(lines, dirIndex, x, y):
    move = True
    while move:
        x = (x + DIRS[dirIndex][0]) % WIDTH
        y = (y + DIRS[dirIndex][1]) % HEIGHT
        if lines[y][x] == '#':
            return None
        move = (lines[y][x] == ' ')
    return (x, y, dirIndex)



# For part 2: using cube with corners at coords (±1, ±1, ±1)
def rotateCube(corners, direction):
    if direction == DOWN:
        matrix = [[ 0,0,1],
                  [ 0,1,0],
                  [-1,0,0]]
    elif direction == LEFT:
        matrix = [[ 0,1,0],
                  [-1,0,0],
                  [ 0,0,1]]
    elif direction == UP:
        matrix = [[0,0,-1],
                  [0,1, 0],
                  [1,0, 0]]
    else:         # RIGHT
        matrix = [[0,-1,0],
                  [1, 0,0],
                  [0, 0,1]]
    return tuple(tuple(sum(line[i]*pt[i] for i in range(3)) for line in matrix) for pt in corners)

def rotateCubeMultiple(directions):
    corners = BASE_FACE
    for direction in directions:
        corners = rotateCube(corners, direction)
    return corners


# Each oriented face is described with UP-LEFT and UP-RIGHT corners.
faceToCoord = {}  # list of coordinates of each oriented face
coordToFace = {}  # list of the faces at given coordinates, orientation is the map one
rotsAtCoord = {}  # list of rotation to get the face from BASE_FACE

# compute and save into dictionaries described on previous lines
def saveCoordsFacesAndRotations(col, row, rotations=None):
    if rotations is None:
        rotations = []
    face = rotateCubeMultiple(rotations)
    for orientation in range(4):
        orientatedFace = tuple((face[orientation:] + face[:orientation])[:2])  # UP-LEFT and UP-RIGHT corners
        faceToCoord[orientatedFace] = (col, row, orientation%4)
    rotsAtCoord[(col,row)] = rotations
    coordToFace[(col,row)] = face

BASE_FACE = ((1,1,1),(1,1,-1),(1,-1,-1),(1,-1,1))

startCol = (START_X - 1) // SIDE_LENGTH
startRow = (START_Y - 1) // SIDE_LENGTH
saveCoordsFacesAndRotations(startCol, startRow)

# Breadth-first search algorithm (I think) to get all the described faces
coordsToProcess = [ (startCol, startRow) ]
while len(coordsToProcess) > 0:
    col, row = coordsToProcess.pop()
    for dirIndex in range(4):
        nextCol = col + DIRS[dirIndex][0]
        nextRow = row + DIRS[dirIndex][1]

        if nextCol < 0 or nextRow < 0 or nextCol >= (WIDTH - 2) // SIDE_LENGTH or nextRow >= (HEIGHT - 2) // SIDE_LENGTH:  # outside the input
            continue

        xInTheMiddle, yInTheMiddle  =  nextCol*SIDE_LENGTH + SIDE_LENGTH//2,  nextRow*SIDE_LENGTH + SIDE_LENGTH//2
        if lines[yInTheMiddle][xInTheMiddle] == ' ':    # no face information written there
            continue

        if (nextCol, nextRow) in coordsToProcess  or  (nextCol, nextRow) in coordToFace:   # alread processed
            continue

        coordsToProcess.append((nextCol, nextRow))
        rotations = [dirIndex] + rotsAtCoord[(col,row)]  # must be done before the others, that's why we have to save the rotations for each face
        saveCoordsFacesAndRotations(nextCol, nextRow, rotations)



EDGE_CONNECTIONS = {}  # tunneling from one edge to another glued to it

# point where we are gluing the edge from
def startPointOfGlueEdge(col, row, dirIndex, gluedFrom=True):
    glueDirIndex = (dirIndex + 1) % len(DIRS)   # turn right
    if gluedFrom:
        offsetDir = glueDirIndex
    else:
        offsetDir = (glueDirIndex - 1) % 4
    offset  =  {  RIGHT:(1, 1),  DOWN:(SIDE_LENGTH, 1),  LEFT:(SIDE_LENGTH, SIDE_LENGTH),  UP:(1, SIDE_LENGTH)  }[offsetDir]
    x = SIDE_LENGTH * col + offset[0]
    y = SIDE_LENGTH * row + offset[1]
    return (x, y, glueDirIndex)

def glueEdges(col1, row1, dirIndex1, col2, row2, dirIndex2):
    global EDGE_CONNECTIONS
    x1,y1,d1 = startPointOfGlueEdge(col1, row1, dirIndex1, gluedFrom=True)
    x2,y2,d2 = startPointOfGlueEdge(col2, row2, dirIndex2, gluedFrom=False)

    # glue each point of edge we glue from to the corresponding one on the other
    for i in range(SIDE_LENGTH):
        nx1, ny1 = x1 + i * DIRS[d1][0], y1 + i * DIRS[d1][1]
        nx2, ny2 = x2 + i * DIRS[d2][0], y2 + i * DIRS[d2][1]
        EDGE_CONNECTIONS[(nx1 + DIRS[dirIndex1][0], ny1 + DIRS[dirIndex1][1], dirIndex1)] = (nx2, ny2, dirIndex2)

### Let's glue the edges and fill EDGE_CONNECTIONS !
for col,row in coordToFace:
    face = coordToFace[(col, row)]
    for sideDirIndex in range(4):  # glue each side of each face, even if we don't need to do already connected ones
        orientedFace = (face+face)[sideDirIndex:sideDirIndex+2]  # repeat the face to avoid doing complicated modulos
        oppositeFace = tuple(reversed(orientedFace))
        col2, row2, indexDir2 = faceToCoord[oppositeFace]
        glueEdges(col, row, sideDirIndex, col2, row2, (indexDir2 + 2) % 4)


### And now it's so even easier to get from one edge to another than with part 1 !
def nextPoint_part2(lines, dirIndex, x, y):
    x = (x + DIRS[dirIndex][0])
    y = (y + DIRS[dirIndex][1])
    if lines[y][x] == ' ':
        x,y,dirIndex = EDGE_CONNECTIONS[(x,y,dirIndex)]

    if lines[y][x] == '#':
        return None
    else:
        return (x,y, dirIndex)




def goStraight(lines, dirIndex, distance, x, y):   # I tried for several years until I accepted myself
    for _ in range(distance):
        nextPt = nextPoint(lines, dirIndex, x, y)
        if nextPt is None:
            break
        x, y, dirIndex  =  nextPt
        lines[y] = lines[y][:x] + SYMBOLS[dirIndex] + lines[y][x+1:]    # for visualization, doesn't change the obstacles '#'
    return (x, y, dirIndex)



### Let's do that cubic walk
for part in [1, 2]:
    x = START_X
    y = START_Y
    dirIndex = RIGHT

    # Let's override the function to get the right one
    if part == 1:
        nextPoint = nextPoint_part1
    else:
        nextPoint = nextPoint_part2

    # Follow the instructions
    for instr in INSTRUCTIONS:
        if instr in {'R', 'L'}:
            if instr == 'R':
                dirIndex = (dirIndex + 1) % len(DIRS)
            elif instr == 'L':
                dirIndex = (dirIndex - 1) % len(DIRS)
        else:
            x, y, dirIndex = goStraight(lines, dirIndex, instr, x, y)


    print("Answer part " + str(part) + ":", 1000*y + 4*x + dirIndex)