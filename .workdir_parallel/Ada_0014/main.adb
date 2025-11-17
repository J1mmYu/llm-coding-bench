with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;

procedure Topological_Sort is
   N, M : Integer;
   
   package Integer_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Integer);
   use Integer_Vectors;
   
   type Adjacency_List is array (Integer range <>) of Integer_Vectors.Vector;
   type Integer_Array is array (Integer range <>) of Integer;
   
   function Solve return Boolean is
      Adj : Adjacency_List (1 .. N);
      In_Degree : Integer_Array (1 .. N) := (others => 0);
      Queue : Integer_Vectors.Vector;
      Result : Integer_Vectors.Vector;
      U, V : Integer;
   begin
      for I in 1 .. M loop
         Get (U);
         Get (V);
         Adj (U).Append (V);
         In_Degree (V) := In_Degree (V) + 1;
      end loop;
      
      for I in 1 .. N loop
         if In_Degree (I) = 0 then
            Queue.Append (I);
         end if;
      end loop;
      
      while not Queue.Is_Empty loop
         declare
            Current : constant Integer := Queue.First_Element;
         begin
            Queue.Delete_First;
            Result.Append (Current);
            
            for I in Adj (Current).First_Index .. Adj (Current).Last_Index loop
               declare
                  Neighbor : constant Integer := Adj (Current).Element (I);
               begin
                  In_Degree (Neighbor) := In_Degree (Neighbor) - 1;
                  if In_Degree (Neighbor) = 0 then
                     Queue.Append (Neighbor);
                  end if;
               end;
            end loop;
         end;
      end loop;
      
      if Integer (Result.Length) /= N then
         return False;
      end if;
      
      for I in Result.First_Index .. Result.Last_Index loop
         if I > 0 then
            Put (" ");
         end if;
         Put (Integer'Image (Result.Element (I))(2 .. Integer'Image (Result.Element (I))'Last));
      end loop;
      New_Line;
      return True;
   end Solve;
   
begin
   Get (N);
   Get (M);
   
   if not Solve then
      Put_Line ("-1");
   end if;
end Topological_Sort;
