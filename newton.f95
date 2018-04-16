PROGRAM newton
  CHARACTER(len=32) :: arg
  REAL :: x, aux
  INTEGER :: i
  i = 1
    CALL getarg(i, arg) !conseguindo os arqumentos de ARG
    READ (arg, *) x     !transformando string para REAL

    aux = x
    x = x/2

    do 20 i = 1, 10, 1
         x = raiz(x, aux)
         WRITE (*,*) x
    20 continue


END PROGRAM newton

FUNCTION fx(x, y) RESULT(num)
REAL :: x, y, num
    num = (x * x) - y
END FUNCTION fx

FUNCTION fxLinha(x) result(num)
REAL :: x, num
    num = 2 * x
END FUNCTION fxLinha

FUNCTION raiz(x, y) RESULT(num)
REAL :: x, y, num
    num = x - (fx(x, y) / fxLinha(x))
END FUNCTION raiz
