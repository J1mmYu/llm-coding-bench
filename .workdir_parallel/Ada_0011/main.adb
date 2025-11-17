with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;

procedure Main is
   N, M : Integer;
   type Grid_Type is array (Positive range <>, Positive range <>) of Character;
   type Visited_Type is array (Positive range <>, Positive range <>) of Boolean;
   
   procedure DFS (Grid : Grid_Type; Visited : in out Visited_Type; I, J : Integer) is
      DI : array (1..4) of Integer := (0, 0, 1, -1);
      DJ : array (1..4) of Integer := (1, -1, 0, 0);
      NI, NJ : Integer;
   begin
      Visited(I, J) := True;
      for K in 1..4 loop
         NI := I + DI(K);
         NJ := J + DJ(K);
         if NI >= 1 and NI <= N and NJ >= 1 and NJ <= M then
            if Grid(NI, NJ) = '.' and not Visited(NI, NJ) then
               DFS(Grid, Visited, NI, NJ);
            end if;
         end if;
      end loop;
   end DFS;
   
begin
   Get(N);
   Get(M);
   Skip_Line;
   
   declare
      Grid : Grid_Type(1..N, 1..M);
      Visited : Visited_Type(1..N, 1..M) := (others => (others => False));
      Count : Integer := 0;
      Line : String(1..M);
   begin
      for I in 1..N loop
         Get_Line(Line, M);
         for J in 1..M loop
            Grid(I, J) := Line(J);
         end loop;
      end loop;
      
      for I in 1..N loop
         for J in 1..M loop
            if Grid(I, J) = '.' and not Visited(I, J) then
               DFS(Grid, Visited, I, J);
               Count := Count + 1;
            end if;
         end loop;
      end loop;
      
      Put_Line(Integer'Image(Count));
   end;
end Main;
