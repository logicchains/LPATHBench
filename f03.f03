Module RouteTypesAndMethods 
  Implicit none

  Type :: Route
    Integer :: Dest
    Integer :: Cost
  END Type Route

  Type :: Node
    Type(Route), Dimension(:), Allocatable :: Neighbors
  End Type Node

  contains

  Recursive Function get_longest_path(nodes,length,node_counts,visited,current_node) Result(distance)
    Implicit none
    Integer, Intent(in)                           :: length
    Type (node), dimension(length), Intent(in)    :: nodes
    Integer, Dimension(length), Intent(in)        :: node_counts
    Logical, Dimension(length), Intent(inout)     :: visited
    Integer, Intent(in)                           :: current_node
    Integer                                       :: distance
    Integer :: I,temp_distance,curmax

    temp_distance=0
    curmax=0

    visited(current_node) = .TRUE.
    DO I=1,node_counts(current_node)
      IF (visited( nodes(current_node)%Neighbors(I)%Dest)) THEN
        CYCLE
      END IF
      temp_distance = nodes(current_node)%Neighbors(I)%Cost + &
        get_longest_path(nodes,length,node_counts,visited,nodes(current_node)%Neighbors(I)%Dest)
      IF (temp_distance > curmax) THEN
        curmax=temp_distance
      END IF
    END DO
    visited(current_node) = .FALSE.
    distance = curmax

  End Function get_longest_path

  Subroutine add_routes_to_neighbors(nodes,length,node_counts)
    Implicit none
    Integer, Intent(in)                           :: length
    Type (Node), Dimension(length), Intent(inout) :: nodes
    Integer, Dimension(length), Intent(in)        :: node_counts

    Integer :: err,temp,node_minus_one,destination_minus_one,cost

    OPEN(UNIT=10,FILE='agraph',STATUS='OLD',ACTION='READ',IOSTAT=err)
    IF (err /= 0) THEN
      WRITE (*,*) "Error code of ",err,"in reading file."
      STOP
    END IF

    READ (10,'(I2)') temp
    temp         = 1

    READ(10,*) node_minus_one,destination_minus_one,cost
    DO WHILE (err == 0)
      ! We could do something crazier here, but luckily file is ordered
      DO temp = 1,node_counts(node_minus_one+1)
        nodes(node_minus_one+1)%Neighbors(temp)%Dest = destination_minus_one + 1
        nodes(node_minus_one+1)%Neighbors(temp)%Cost = cost   
        READ(10,*,IOSTAT=err) node_minus_one,destination_minus_one,cost
        IF (err /= 0) THEN 
          EXIT 
        END IF
      END DO

    ENDDO
    CLOSE(10)

  End Subroutine add_routes_to_neighbors
  
  Subroutine populate_neighbors(nodes,length,node_counts)
    Implicit none
    Integer, Intent(in) :: length
    Type (Node), Intent(inout), Dimension(length) :: nodes 
    Integer :: I=0,err,node_minus_one,neighbor,cost
    Integer, Dimension(1:length), Intent(out) :: node_counts
    ! Initialize all of node_counts to zero
    node_counts = 0
    OPEN(UNIT=10,FILE='agraph',STATUS='OLD',ACTION='READ',IOSTAT=err)
    IF (err /= 0) THEN
      WRITE (*,*) "Err of ",err," in reading the file."
      STOP
    END IF

    READ (10,'(I2)') I

    READ(10,*,IOSTAT=err) node_minus_one,neighbor,cost


    DO WHILE (err == 0)
      node_counts(node_minus_one+1) = node_counts(node_minus_one+1)+1
      READ(10,*,IOSTAT=err) node_minus_one,neighbor,cost
    END DO

    AllocateNeighbors: DO I=1,length
      Allocate(nodes(I)%Neighbors(node_counts(I)))
    END DO AllocateNeighbors
    CLOSE(10)
  End Subroutine populate_neighbors

  Subroutine populate_node_array(nodes,total_node_count)
    Implicit none
    Type (Node), Intent(out), Dimension(:), Allocatable  :: nodes
    Integer,     Intent(out)                             :: total_node_count
    Call get_total_nodes(total_node_count)
    If (total_node_count == -1) Then
      WRITE (*,*) "Error reading file - recompile to get error message"
      STOP
    END IF
    ALLOCATE(nodes(total_node_count))
  End Subroutine populate_node_array

  Subroutine get_total_nodes(node_count)
    Implicit none
    Integer, intent(out) :: node_count
    Integer :: err
    OPEN(UNIT=10,FILE='agraph',STATUS='OLD',ACTION='READ',IOSTAT=err)
    READ(10,'(I2)') node_count
    IF (err /= 0) THEN
      node_count = -1
    END IF
    CLOSE(10)
  End Subroutine get_total_nodes
End Module RouteTypesAndMethods

PROGRAM LONGESTROUTE
  Use RouteTypesAndMethods
  Implicit None
  Integer :: total_node_count=0
  Integer, Dimension(:), Allocatable :: node_lengths
  Integer :: I=0
  Integer :: max_distance=0
  Real    :: start_time=0.,end_time=0.
  Type (Node), Dimension(:), Allocatable :: Nodes
  Logical, Dimension(:), Allocatable     :: visited

  ! Read first line and allocate nodes
  Call populate_node_array(Nodes,total_node_count)
  Allocate(node_lengths(total_node_count))
  Allocate(visited(total_node_count))
  node_lengths = 0
  visited      = .FALSE.
  ! Read rest of file and allocate lengths for each node's neighbor array
  Call populate_neighbors(Nodes,total_node_count,node_lengths)
  Call add_routes_to_neighbors(Nodes,total_node_count,node_lengths)
  I = 1
  Call CPU_TIME(start_time)
  !Recursive Function get_longest_path(nodes,length,node_counts,visited,current_node) Result(distance)
  max_distance = get_longest_path(Nodes,total_node_count,node_lengths,visited,I)
  Call CPU_TIME(end_time)
  write (*,'(I4," LANGUAGE Fortran_2003 ",I4)') max_distance, INT((end_time-start_time)*1000)
END PROGRAM LONGESTROUTE
