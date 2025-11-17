with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;

procedure Main is
   type Interval is record
      L, R : Integer;
   end record;
   
   package Interval_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Interval);
   use Interval_Vectors;
   
   N : Integer;
   Intervals : Vector;
   
   procedure Sort_Intervals (V : in out Vector) is
      Temp : Interval;
      Min_Idx : Natural;
   begin
      for I in 0 .. Natural(V.Length) - 1 loop
         Min_Idx := I;
         for J in I + 1 .. Natural(V.Length) - 1 loop
            if V(J).L < V(Min_Idx).L then
               Min_Idx := J;
            end if;
         end loop;
         if Min_Idx /= I then
            Temp := V(I);
            V(I) := V(Min_Idx);
            V(Min_Idx) := Temp;
         end if;
      end loop;
   end Sort_Intervals;
   
   L, R : Integer;
   Total_Length : Integer := 0;
   Current_L, Current_R : Integer;
   
begin
   N := Integer'Value(Get_Line);
   
   for I in 1 .. N loop
      declare
         Line : String := Get_Line;
         Space_Pos : Natural := 0;
      begin
         for J in Line'Range loop
            if Line(J) = ' ' then
               Space_Pos := J;
               exit;
            end if;
         end loop;
         L := Integer'Value(Line(Line'First .. Space_Pos - 1));
         R := Integer'Value(Line(Space_Pos + 1 .. Line'Last));
         Intervals.Append((L, R));
      end;
   end loop;
   
   Sort_Intervals(Intervals);
   
   if Natural(Intervals.Length) > 0 then
      Current_L := Intervals(0).L;
      Current_R := Intervals(0).R;
      
      for I in 1 .. Natural(Intervals.Length) - 1 loop
         if Intervals(I).L <= Current_R then
            if Intervals(I).R > Current_R then
               Current_R := Intervals(I).R;
            end if;
         else
            Total_Length := Total_Length + (Current_R - Current_L);
            Current_L := Intervals(I).L;
            Current_R := Intervals(I).R;
         end if;
      end loop;
      
      Total_Length := Total_Length + (Current_R - Current_L);
   end if;
   
   Put_Line(Integer'Image(Total_Length));
end Main;
