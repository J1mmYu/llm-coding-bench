with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;

procedure Longest_Increasing_Subsequence is
   package Int_IO is new Ada.Text_IO.Integer_IO(Integer);
   use Int_IO;
   
   package Int_Vectors is new Ada.Containers.Vectors(Positive, Integer);
   use Int_Vectors;
   
   N : Integer;
   Arr : Vector;
   Temp : Integer;
   
   function LIS_Length return Integer is
      Tails : Vector;
      Pos : Integer;
      Left, Right, Mid : Integer;
   begin
      if Is_Empty(Arr) then
         return 0;
      end if;
      
      for I in First_Index(Arr) .. Last_Index(Arr) loop
         Temp := Element(Arr, I);
         
         if Is_Empty(Tails) or else Temp > Last_Element(Tails) then
            Append(Tails, Temp);
         else
            Left := First_Index(Tails);
            Right := Last_Index(Tails);
            Pos := Left;
            
            while Left <= Right loop
               Mid := Left + (Right - Left) / 2;
               if Element(Tails, Mid) < Temp then
                  Left := Mid + 1;
               else
                  Pos := Mid;
                  Right := Mid - 1;
               end if;
            end loop;
            
            Replace_Element(Tails, Pos, Temp);
         end if;
      end loop;
      
      return Integer(Length(Tails));
   end LIS_Length;
   
begin
   Get(N);
   
   for I in 1 .. N loop
      Get(Temp);
      Append(Arr, Temp);
   end loop;
   
   Put(LIS_Length, Width => 1);
   New_Line;
end Longest_Increasing_Subsequence;
