with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

procedure Word_Frequency is
   use Ada.Text_IO;
   use Ada.Strings.Unbounded;

   package String_Integer_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Integer,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=");

   type Word_Count is record
      Word  : Unbounded_String;
      Count : Integer;
   end record;

   package Word_Count_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Word_Count);

   function Less_Than (Left, Right : Word_Count) return Boolean is
   begin
      if Left.Count /= Right.Count then
         return Left.Count > Right.Count;  -- Sort by -count (descending)
      else
         return To_String (Left.Word) < To_String (Right.Word);  -- Then by word
      end if;
   end Less_Than;

   package Word_Count_Sorting is new Word_Count_Vectors.Generic_Sorting ("<" => Less_Than);

   Line   : String := Get_Line;
   Map    : String_Integer_Maps.Map;
   Vec    : Word_Count_Vectors.Vector;
   I      : Positive := Line'First;
   J      : Positive;
   Word   : Unbounded_String;

begin
   -- Parse words
   while I <= Line'Last loop
      -- Skip non-alphabetic characters
      while I <= Line'Last and then not (Line (I) in 'A' .. 'Z' | 'a' .. 'z') loop
         I := I + 1;
      end loop;

      if I <= Line'Last then
         J := I;
         while J <= Line'Last and then (Line (J) in 'A' .. 'Z' | 'a' .. 'z') loop
            J := J + 1;
         end loop;

         Word := To_Unbounded_String (Line (I .. J - 1));
         
         if Map.Contains (To_String (Word)) then
            Map (To_String (Word)) := Map (To_String (Word)) + 1;
         else
            Map.Insert (To_String (Word), 1);
         end if;

         I := J;
      end if;
   end loop;

   -- Convert map to vector
   for C in Map.Iterate loop
      Vec.Append (Word_Count'(Word  => To_Unbounded_String (String_Integer_Maps.Key (C)),
                              Count => String_Integer_Maps.Element (C)));
   end loop;

   -- Sort
   Word_Count_Sorting.Sort (Vec);

   -- Print
   for WC of Vec loop
      Put_Line (To_String (WC.Word) & " " & Integer'Image (WC.Count)(2 .. Integer'Image (WC.Count)'Last));
   end loop;
end Word_Frequency;
