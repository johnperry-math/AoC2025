pragma Ada_2022;

with Ada.Containers.Vectors;
with Ada.Text_IO;

procedure Day09 is

   package IO renames Ada.Text_IO;
   package Natural_IO is new IO.Integer_IO (Num => Natural);

   Debug : constant Boolean := False;

   type Location_Record is record
      Row, Col : Natural;
   end record;

   package Position_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Location_Record);
   subtype Position_Vector is Position_Vectors.Vector;
   Tile_Locations : Position_Vector;

   procedure Read_Input is
      Input : IO.File_Type;
   begin
      IO.Open
        (Input, IO.In_File, (if Debug then "example.txt" else "input.txt"));
      while not IO.End_Of_File (Input) loop
         declare
            Line     : constant String := IO.Get_Line (Input);
            Location : Location_Record;
            Position : Positive := Line'First;
         begin
            Natural_IO.Get (Line, Location.Col, Position);
            Natural_IO.Get
              (Line (Position + 2 .. Line'Last), Location.Row, Position);
            Tile_Locations.Append (Location);
         end;
      end loop;
      IO.Close (Input);
   end Read_Input;

   type Area_Size is range 0 .. 2**64 - 1;

   function Part_1 return Area_Size is
      Result : Area_Size := 0;
   begin
      for Ith in Tile_Locations.First_Index .. Tile_Locations.Last_Index loop
         for Jth in Ith + 1 .. Tile_Locations.Last_Index loop
            declare
               Height    : constant Area_Size :=
                 Area_Size
                   (abs (Tile_Locations (Ith).Row
                         - Tile_Locations (Jth).Row
                         + 1));
               Width     : constant Area_Size :=
                 Area_Size
                   (abs (Tile_Locations (Ith).Col
                         - Tile_Locations (Jth).Col
                         + 1));
               This_Area : constant Area_Size := Height * Width;
            begin
               if This_Area > Result then
                  Result := This_Area;
               end if;
            end;
         end loop;
      end loop;
      return Result;
   end Part_1;

   type Edge_Record is record
      Start, Finish : Location_Record;
   end record;

   function Is_Horizontal (Edge : Edge_Record) return Boolean
   is (Edge.Start.Row = Edge.Finish.Row);

   function Right_Ray_Crosses_Edge
     (X_Origin, Y_Origin : Float; Edge : Edge_Record) return Boolean is
   begin
      if Is_Horizontal (Edge) then
         return False;
      else
         return
           X_Origin < Float (Edge.Start.Col)
           and then Y_Origin
                    >= Float (Natural'Min (Edge.Start.Row, Edge.Finish.Row))
           and then Y_Origin
                    <= Float (Natural'Max (Edge.Start.Row, Edge.Finish.Row));
      end if;
   end Right_Ray_Crosses_Edge;

   package Edge_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Edge_Record);
   subtype Edge_Vector is Edge_Vectors.Vector;
   All_Edges : Edge_Vector;

   function One_New_Edge (Current, Next : Location_Record) return Edge_Record
   is
      Edge : Edge_Record;
   begin
      Edge.Start := Current;
      Edge.Finish := Next;
      return Edge;
   end One_New_Edge;

   procedure Make_Edges is
   begin
      for Ith in Tile_Locations.First_Index .. Tile_Locations.Last_Index - 1
      loop
         All_Edges.Append
           (One_New_Edge (Tile_Locations (Ith), Tile_Locations (Ith + 1)));
      end loop;
      All_Edges.Append
        (One_New_Edge
           (Tile_Locations (Tile_Locations.Last_Index),
            Tile_Locations (Tile_Locations.First_Index)));
   end Make_Edges;

   function Shape_Contains_Point (X_Origin, Y_Origin : Float) return Boolean is
      Count : Natural := 0;
      Edge  : Edge_Record;
   begin
      for Ith in All_Edges.First_Index .. All_Edges.Last_Index loop
         Edge := All_Edges (Ith);
         if Right_Ray_Crosses_Edge (X_Origin, Y_Origin, Edge) then
            Count := @ + 1;
         end if;
      end loop;
      return Count mod 2 = 1;
   end Shape_Contains_Point;

   function Shape_Contains_Corners
     (First, Second : Location_Record) return Boolean
   is
      Min_Row, Max_Row, Min_Col, Max_Col : Float;
   begin
      Min_Row := Float (Natural'Min (First.Row, Second.Row)) + 0.5;
      Max_Row := Float (Natural'Max (First.Row, Second.Row)) - 0.5;
      Min_Col := Float (Natural'Min (First.Col, Second.Col)) + 0.5;
      Max_Col := Float (Natural'Max (First.Col, Second.Col)) - 0.5;
      return
        Shape_Contains_Point (Min_Col, Min_Row)
        and then Shape_Contains_Point (Min_Col, Max_Row)
        and then Shape_Contains_Point (Max_Col, Min_Row)
        and then Shape_Contains_Point (Max_Col, Max_Row);
   end Shape_Contains_Corners;

   function Cuts (First, Second : Edge_Record) return Boolean is
      Smaller_First, Larger_First   : Natural;
      Smaller_Second, Larger_Second : Natural;
   begin
      if Is_Horizontal (First) = Is_Horizontal (Second) then
         return False;
      end if;
      if Is_Horizontal (First) then
         Smaller_First := Natural'Min (First.Start.Col, First.Finish.Col);
         Larger_First := Natural'Max (First.Start.Col, First.Finish.Col);
         Smaller_Second := Natural'Min (Second.Start.Row, Second.Finish.Row);
         Larger_Second := Natural'Max (Second.Start.Row, Second.Finish.Row);
         return
           Second.Start.Col in Smaller_First + 1 .. Larger_First - 1
           and then First.Start.Row in Smaller_Second + 1 .. Larger_Second - 1;
      else
         Smaller_First := Natural'Min (First.Start.Row, First.Finish.Row);
         Larger_First := Natural'Max (First.Start.Row, First.Finish.Row);
         Smaller_Second := Natural'Min (Second.Start.Col, Second.Finish.Col);
         Larger_Second := Natural'Max (Second.Start.Col, Second.Finish.Col);
         return
           Second.Start.Row in Smaller_First + 1 .. Larger_First - 1
           and then First.Start.Col in Smaller_Second + 1 .. Larger_Second - 1;
      end if;
   end Cuts;

   function No_Cut_Of (Edge : Edge_Record) return Boolean
   is (for all Other of All_Edges => not Cuts (Edge, Other));

   function No_Cut_Of
     (Min_Row, Min_Col, Max_Row, Max_Col : Natural) return Boolean
   is
      Edge_1 : constant Edge_Record :=
        ((Min_Row, Min_Col), (Min_Row, Max_Col));
      Edge_2 : constant Edge_Record :=
        ((Min_Row, Max_Col), (Max_Row, Max_Col));
      Edge_3 : constant Edge_Record :=
        ((Max_Row, Max_Col), (Max_Row, Min_Col));
      Edge_4 : constant Edge_Record :=
        ((Max_Row, Min_Col), (Min_Row, Min_Col));
   begin
      return
        No_Cut_Of (Edge_1)
        and then No_Cut_Of (Edge_2)
        and then No_Cut_Of (Edge_3)
        and then No_Cut_Of (Edge_4);
   end No_Cut_Of;

   function Shape_Is_Not_Cut (First, Second : Location_Record) return Boolean
   is
      Min_Row, Max_Row, Min_Col, Max_Col : Natural;
   begin
      Min_Row := Natural'Min (First.Row, Second.Row);
      Max_Row := Natural'Max (First.Row, Second.Row);
      Min_Col := Natural'Min (First.Col, Second.Col);
      Max_Col := Natural'Max (First.Col, Second.Col);
      return No_Cut_Of (Min_Row, Min_Col, Max_Row, Max_Col);
   end Shape_Is_Not_Cut;

   function Part_2 return Area_Size is
      Result        : Area_Size := 0;
      First, Second : Location_Record;
   begin
      for Ith in Tile_Locations.First_Index .. Tile_Locations.Last_Index loop
         First := Tile_Locations (Ith);
         for Jth in Ith + 1 .. Tile_Locations.Last_Index loop
            Second := Tile_Locations (Jth);
            if Shape_Contains_Corners (First, Second)
              and then Shape_Is_Not_Cut (First, Second)
            then
               declare
                  Height    : constant Area_Size :=
                    Area_Size (abs (First.Row - Second.Row) + 1);
                  Width     : constant Area_Size :=
                    Area_Size (abs (First.Col - Second.Col) + 1);
                  This_Area : constant Area_Size := Height * Width;
               begin
                  if This_Area > Result then
                     Result := This_Area;
                  end if;
               end;
            end if;
         end loop;
      end loop;
      return Result;
   end Part_2;

begin
   Read_Input;
   IO.Put_Line ("The rectangle of greatest area would be" & Part_1'Image);
   Make_Edges;
   IO.Put_Line
     ("The red/green rectangle of greatest area would be" & Part_2'Image);
end Day09;
