PROGRAM HeatedPlate
IMPLICIT NONE

  !I,J: INDEXERS
  !NX, NY: GRID SIZE
  !EPOCH: NUM ITERATIONS TO CONVERGENCE
  !T: PLATE WITH GHOST POINTS
  !W: DUMMY PLATE TO UPDATE T (NO GHOST POINTS)
  !SC: STOPPING CONDITION IN KELVIN
  !SUM: AVG TEMPERATURE OF PLATE
  !FT: VARIABLE TO EXIT WHILE LOOP
  INTEGER :: I, J, NX, NY, EPOCH
  REAL :: SC, SUM
  REAL, DIMENSION(:,:), ALLOCATABLE :: T, W
  LOGICAL :: FT

  !GRID SIZE
  PRINT *, 'ENTER A GRID SIZE THAT WILL MAKE YE TRIP OUT (and in the 10s place ;] )'
  READ(*, FMT='(I2)') NX, NY

  !CREATE GRID
  ALLOCATE(T(NY+2, NX+2), W(NY, NX))

  !STOPPING CONDITION VARIABLES
  FT = .TRUE.
  EPOCH = 0
  SC = 0.001

  !SET GHOST POINTS TO 0 KELVIN
  DO J=1, NX+2
    T(1, J)=0
    T(NX+1, J)=0
  END DO

  DO I=1, NY+2
    T(I, 1)=0
    T(I, NY+1)=0
  END DO

  !POPULATE TEMPERATURE ARRAY T: SIN((X^2+Y^2)^0.5)
  DO J=2,NX
    DO I=2,NY
      T(I,J)=(SIN(((I**2)+(J**2))**0.5)**2)*1000
    END DO
  END DO
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !OPEN FILE
  !DATA FILE
  OPEN(UNIT=1,STATUS='REPLACE', FILE = 'hPlate_Data.txt')
  !WRITES IN INITIAL PLATE TO DATA FILE
  DO I=2,NY
    DO J=2,NX
      WRITE(1,900, ADVANCE='NO') T(I,J)
      900 FORMAT (F14.8)
      WRITE(1, FMT = '(A)', advance='no') ','
    END DO
    WRITE(1,*)
  END DO

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !EXECUTE HEAT TRANSFER UNTIL PLATE CONVERGES
  print *, 'COMPUTING'
  DO WHILE (FT)
    EPOCH = EPOCH+1
    !COMPUTE HEAT TRANSFER

    DO J=2,NX
      DO I=2,NY
        W(I,J) = (T(I-1, J)+T(I+1, J)+T(I, J-1)+T(I, J+1))/4
      END DO
    END DO

    !UPDATE T WITH W
    DO J=2,NX
      DO I=2,NY
          T(I,J) = W(I,J)
      END DO
    END DO

    !STOPPING CONDITION
    DO J=2,NX
      DO I=2,NY
          SUM = SUM+W(I,J)
      END DO
    END DO
    SUM = SUM/(NX*NY)

    IF (SUM <= SC)THEN
      FT = .FALSE.
    END IF

    !ELEVATOR MUSIC
    IF (MOD(EPOCH,100)==0)THEN
      WRITE(*, FMT = '(A)', advance='no') '.'
    END IF

    !WRITE NEW EPOCH INTO DATA FILE
    DO I=2,NY
      DO J=2,NX
        WRITE(1,901, ADVANCE='NO') T(I,J)
        901 FORMAT (F14.8)
        WRITE(1, FMT = '(A)', advance='no') ','
      END DO
      WRITE(1,*)
    END DO

  END DO

  CLOSE(1)

  !SAVE THE OF SHAPE FILE
  OPEN(UNIT=2,STATUS='REPLACE', FILE = 'shape.txt')
  IF (NX<=9 .AND. NY<=9)THEN
    WRITE(2,FMT='(I1)') NX, NY

  ELSE IF (NX<=99 .AND. NX>=10 .AND. NY<=9)THEN
    WRITE(2,FMT='(I2)') NX
    WRITE(2,FMT='(I1)') NY

  ELSE IF (NX<=9 .AND. NY<=99 .AND. NY>=10)THEN
    WRITE(2,FMT='(I1, I2)') NX
    WRITE(2,FMT='(I2)') NY

  ELSE IF (NX<=99 .AND. NY<=99)THEN
    WRITE(2,FMT='(I2)') NX, NY

  END IF

  !FORMAT EPOCH TO PRINT OUT BASED ON NUM EPOCHs
  IF (EPOCH>=1000) THEN
    WRITE(2,FMT='(I4)') EPOCH
  ELSE IF (EPOCH>=100) THEN
    WRITE(2,FMT='(I3)') EPOCH
  ELSE IF (EPOCH>=10) THEN
    WRITE(2,FMT='(I2)') EPOCH
  ELSE IF (EPOCH>=10) THEN
    WRITE(2,FMT='(I1)') EPOCH
  END IF

  CLOSE(2)

  print *, ' '
  print *, 'DONE'

END PROGRAM
