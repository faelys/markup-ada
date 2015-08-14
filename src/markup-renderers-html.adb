------------------------------------------------------------------------------
-- Copyright (c) 2013-2015, Natacha Porté                                   --
--                                                                          --
-- Permission to use, copy, modify, and distribute this software for any    --
-- purpose with or without fee is hereby granted, provided that the above   --
-- copyright notice and this permission notice appear in all copies.        --
--                                                                          --
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES --
-- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF         --
-- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR  --
-- ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES   --
-- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN    --
-- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF  --
-- OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.           --
------------------------------------------------------------------------------

with Ada.Characters.Latin_1;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;

package body Markup.Renderers.Html is

   package Fixed renames Ada.Strings.Fixed;
   package L1 renames Ada.Characters.Latin_1;
   package Maps renames Ada.Strings.Maps;
   package Slices renames Natools.String_Slices;


   ------------------------------
   -- Local helper subprograms --
   ------------------------------

   function Create
     (Renderer : Renderer_Ref;
      Tag : String;
      Newline_Before_Open : Boolean := False;
      Newline_After_Open : Boolean := False;
      Newline_Before_Close : Boolean := False;
      Newline_After_Close : Boolean := False;
      Self_Closing : Boolean := False)
      return Html_Element;

   function Create
     (Renderer : Renderer_Ref;
      First_Tag : String;
      Second_Tag : String;
      Newline_Before_Open : Boolean := False;
      Newline_After_Open : Boolean := False;
      Newline_Before_Close : Boolean := False;
      Newline_After_Close : Boolean := False)
      return Html_Double_Element;

   function Image (A : Alignment) return String;



   function Create
     (Renderer : Renderer_Ref;
      Tag : String;
      Newline_Before_Open : Boolean := False;
      Newline_After_Open : Boolean := False;
      Newline_Before_Close : Boolean := False;
      Newline_After_Close : Boolean := False;
      Self_Closing : Boolean := False)
     return Html_Element is
   begin
      return Html_Element'(
         Tag => Tag & Fixed."*" (Html_Tag_Max_Size - Tag'Length, ' '),
         Tag_Size => Tag'Length,
         Newline_Before_Open => Newline_Before_Open,
         Newline_After_Open => Newline_After_Open,
         Newline_Before_Close => Newline_Before_Close,
         Newline_After_Close => Newline_After_Close,
         Self_Closing => Self_Closing,
         Classes => Ada.Strings.Unbounded.Null_Unbounded_String,
         Id => <>,
         Align => <>,
         Opened => False,
         Renderer => Renderer);
   end Create;


   function Create
     (Renderer : Renderer_Ref;
      First_Tag : String;
      Second_Tag : String;
      Newline_Before_Open : Boolean := False;
      Newline_After_Open : Boolean := False;
      Newline_Before_Close : Boolean := False;
      Newline_After_Close : Boolean := False)
     return Html_Double_Element is
   begin
      return Html_Double_Element'(
         Create
           (Renderer,
            First_Tag,
            Newline_Before_Open,
            Newline_After_Open,
            Newline_Before_Close,
            Newline_After_Close,
            Self_Closing => False)
         with
            Second_Tag => Second_Tag
              & Fixed."*" (Html_Tag_Max_Size - Second_Tag'Length, ' '),
            Second_Size => Second_Tag'Length);
   end Create;


   function Image (A : Alignment) return String is
   begin
      case A is
         when Default_Align => return "";
         when Left_Aligned  => return "text-align: left";
         when Centered_Text => return "text-align: center";
         when Right_Aligned => return "text-align: right";
      end case;
   end Image;



   ----------------------------------
   -- HTML renderer internal state --
   ----------------------------------

   function Get_Format (Renderer : Renderer_Ref) return Output_Format is
   begin
      return Renderer.Query.Data.Format;
   end Get_Format;


   procedure Set_Format (Renderer : in out Renderer_Ref;
                         Format : Output_Format) is
   begin
      Renderer.Update.Data.Format := Format;
   end Set_Format;


   function Get_Newline (Renderer : Renderer_Ref) return Newline_Format is
   begin
      return Renderer.Query.Data.Newline;
   end Get_Newline;


   procedure Set_Newline (Renderer : in out Renderer_Ref;
                          Format : Newline_Format) is
   begin
      Renderer.Update.Data.Newline := Format;
   end Set_Newline;


   function Get_Output (Renderer : Renderer_Ref) return Accumulator is
   begin
      return Renderer.Query.Data.Output;
   end Get_Output;

   procedure Set_Output (Renderer : in out Renderer_Ref;
                         Output : in Accumulator)
   is
      function Factory return Renderer_Data;

      function Factory return Renderer_Data is
      begin
         return Renderer_Data'(others => <>);
      end Factory;
   begin
      if Renderer.Is_Empty then
         Renderer.Replace (Factory'Access);
      end if;
      Renderer.Update.Data.Output := Output;
   end Set_Output;



   ---------------------
   -- Rendering tools --
   ---------------------

   procedure Append_Attribute
     (State : in out Renderer_Data;
      Name : in String;
      Value : in String;
      Allow_Empty : in Boolean := False) is
   begin
      if not Allow_Empty and then Value = "" then
         return;
      end if;

      State.Beginning_Of_Line := False;
      Append (State.Output, " ");
      Append (State.Output, Name);
      Append (State.Output, "=""");
      Append_Text (State, Value, Escape_Quotes => True);
      Append (State.Output, """");
   end Append_Attribute;


   procedure Append_Text
     (State : in out Renderer_Data;
      Text : in String;
      Escape_Quotes : in Boolean := False)
   is
      Escape_Set : Maps.Character_Set;
      Position : Positive := Text'First;
      N : Natural;
      Last : Natural := Text'Last;
   begin
      if Last in Text'Range then
         if Text (Last) = L1.CR then
            if Last - 1 in Text'Range and then Text (Last - 1) = L1.LF then
               Last := Last - 2;
               State.Autodetected := LF_CR;
            else
               Last := Last - 1;
               State.Autodetected := CR;
            end if;
         elsif Text (Last) = L1.LF then
            if Last - 1 in Text'Range and then Text (Last - 1) = L1.CR then
               Last := Last - 2;
               State.Autodetected := CR_LF;
            else
               Last := Last - 1;
               State.Autodetected := LF;
            end if;
         end if;
      end if;

      Append_Pending_Newline (State);
      State.Newline_Pending := Last < Text'Last;

      if Escape_Quotes then
         Escape_Set := Maps.To_Set ("""<>&");
      else
         Escape_Set := Maps.To_Set ("<>&");
      end if;

      while Position in Text'First .. Last loop
         N := Fixed.Index (Source => Text,
                           Set => Escape_Set,
                           From => Position,
                           Test => Ada.Strings.Inside);
         if N = 0 then
            N := Last + 1;
         end if;
         if Position < N then
            Append (State.Output, Text (Position .. N - 1));
            State.Beginning_Of_Line := False;
         end if;
         Position := N;
         exit when Position not in Text'First .. Last;
         N := Fixed.Index (Source => Text,
                           Set => Escape_Set,
                           From => Position,
                           Test => Ada.Strings.Outside);
         if N = 0 then
            N := Last + 1;
         end if;
         if Position < N then
            for J in Position .. N - 1 loop
               case Text (J) is
                  when '"' => Append (State.Output, "&quot;");
                  when '&' => Append (State.Output, "&amp;");
                  when '<' => Append (State.Output, "&lt;");
                  when '>' => Append (State.Output, "&gt;");
                  when others => raise Program_Error;
               end case;
               State.Beginning_Of_Line := False;
            end loop;
         end if;
         Position := N;
      end loop;
   end Append_Text;


   procedure Append_Indent (State : in out Renderer_Data) is
   begin
      if State.Beginning_Of_Line and State.Indent_Level > 0 then
         Append (State.Output, Fixed."*" (State.Indent_Level, "  "));
         State.Beginning_Of_Line := False;
      end if;
   end Append_Indent;


   procedure Append_Newline (State : in out Renderer_Data) is
      Marker : Newline_Format := State.Newline;
   begin
      if State.Newline = Autodetect then
         Marker := State.Autodetected;
      end if;

      case Marker is
         when CR    => Append (State.Output, (1 => L1.CR));
         when LF    => Append (State.Output, (1 => L1.LF));
         when CR_LF => Append (State.Output, L1.CR & L1.LF);
         when LF_CR => Append (State.Output, L1.LF & L1.CR);
         when Autodetect => null;
      end case;

      State.Newline_Pending := False;
      State.Beginning_Of_Line := True;
   end Append_Newline;


   procedure Append_Pending_Newline (State : in out Renderer_Data) is
   begin
      if State.Newline_Pending then
         Append_Newline (State);
      end if;
   end Append_Pending_Newline;


   procedure Update_Indent
     (State : in out Renderer_Data;
      Level_Change : in Integer) is
   begin
      State.Indent_Level := State.Indent_Level + Level_Change;
   end Update_Indent;



   -----------------
   -- Base tokens --
   -----------------

   overriding procedure Open (Element : in out Html_Element) is
      Renderer : constant Renderer_Refs.Mutator := Element.Renderer.Update;
   begin
      if Element.Opened then
         raise Program_Error with "Opening an already-opened token";
      end if;

      if Element.Newline_Before_Open
        and then not Renderer.Beginning_Of_Line
      then
         Renderer.Append_Newline;
      else
         Renderer.Append_Pending_Newline;
      end if;

      Renderer.Append_Indent;

      Append
        (Renderer.Output,
         "<" & Element.Tag (1 .. Positive (Element.Tag_Size)));

      Append_Attributes (Html_Element'Class (Element));

      if not Element.Self_Closing then
         Append (Renderer.Output, ">");
      end if;

      if Element.Newline_After_Open then
         Renderer.Append_Newline;
      end if;

      Renderer.Update_Indent (+1);
      Element.Opened := True;
   end Open;


   overriding procedure Close (Element : in out Html_Element) is
      Renderer : constant Renderer_Refs.Mutator := Element.Renderer.Update;
   begin
      if not Element.Opened then
         raise Program_Error with "Closing a non-opened token";
      end if;

      Renderer.Update_Indent (-1);

      if Element.Newline_Before_Close
        and then not Renderer.Beginning_Of_Line
      then
         Renderer.Append_Newline;
      end if;

      Renderer.Append_Indent;

      if Element.Self_Closing then
         case Renderer.Format is
            when Html =>
               Append (Renderer.Output, ">");
            when Xhtml =>
               Append (Renderer.Output, " />");
         end case;
      else
         Append
           (Renderer.Output,
            "</" & Element.Tag (1 .. Positive (Element.Tag_Size)) & ">");
      end if;

      if Element.Newline_After_Close then
         Renderer.Append_Newline;
      end if;

      Element.Opened := False;
   end Close;


   overriding procedure Append
     (Element : in out Html_Element;
      Text : in String) is
   begin
      if not Element.Opened then
         raise Program_Error with "Append text to a non-opened token";
      end if;

      if not Element.Self_Closing then
         Element.Renderer.Update.Data.Append_Text (Text);
      end if;
   end Append;


   procedure Append_Attributes (Element : in out Html_Element) is
      Renderer : constant Renderer_Refs.Mutator := Element.Renderer.Update;
   begin
      Renderer.Append_Attribute
        ("id", Slices.To_String (Element.Id));

      Renderer.Append_Attribute
        ("class", Ada.Strings.Unbounded.To_String (Element.Classes));

      Renderer.Append_Attribute
        ("style", Get_Style (Html_Element'Class (Element)));
   end Append_Attributes;


   function Get_Style (Element : in Html_Element) return String is
   begin
      return Image (Element.Align);
   end Get_Style;


   overriding procedure Add_Class
     (Element : in out Html_Element;
      Class : in Natools.String_Slices.Slice) is
   begin
      if Element.Opened then
         raise Program_Error with "Modification of an opened token";
      end if;

      if Ada.Strings.Unbounded.Length (Element.Classes) > 0 then
         Ada.Strings.Unbounded.Append (Element.Classes, ' ');
      end if;

      Ada.Strings.Unbounded.Append (Element.Classes, Slices.To_String (Class));
   end Add_Class;


   overriding procedure Reset_Class_List (Element : in out Html_Element) is
   begin
      if Element.Opened then
         raise Program_Error with "Modification of an opened token";
      end if;

      Element.Classes := Ada.Strings.Unbounded.Null_Unbounded_String;
   end Reset_Class_List;


   overriding procedure Set_Alignment
     (Element : in out Html_Element;
      Align : in Alignment) is
   begin
      if Element.Opened then
         raise Program_Error with "Modification of an opened token";
      end if;

      Element.Align := Align;
   end Set_Alignment;


   overriding procedure Set_Id
     (Element : in out Html_Element;
      Name : in Natools.String_Slices.Slice) is
   begin
      if Element.Opened then
         raise Program_Error with "Modification of an opened token";
      end if;

      Element.Id := Name;
   end Set_Id;



   ------------------
   -- Header token --
   ------------------

   overriding procedure Set_Level
     (Element : in out Html_Header;
      Level : in Positive)
   is
      New_Tag : String := Positive'Image (Level);
   begin
      if Element.Opened then
         raise Program_Error with "Modification of an opened token";
      end if;

      New_Tag (New_Tag'First) := 'h';
      Element.Tag (1 .. New_Tag'Length) := New_Tag;
      Element.Tag (New_Tag'Length + 1 .. Element.Tag'Last) := (others => ' ');
      Element.Tag_Size := New_Tag'Length;
   end Set_Level;



   ----------------------
   -- Token with title --
   ----------------------

   overriding procedure Set_Title
     (Element : in out Html_Titled;
      Title : in Natools.String_Slices.Slice) is
   begin
      if Element.Opened then
         raise Program_Error with "Modification of an opened token";
      end if;

      Element.Title := Title;
   end Set_Title;


   overriding procedure Append_Attributes (Element : in out Html_Titled) is
   begin
      Append_Attributes (Html_Element (Element));

      if not Element.Title.Is_Null then
         Element.Renderer.Update.Data.Append_Attribute
           ("title", Slices.To_String (Element.Title), Allow_Empty => True);
      end if;
   end Append_Attributes;



   ------------------
   -- Anchor token --
   ------------------

   overriding procedure Set_Link
     (Element : in out Html_Anchor;
      Link : in Natools.String_Slices.Slice) is
   begin
      if Element.Opened then
         raise Program_Error with "Modification of an opened token";
      end if;

      Element.Link := Link;
   end Set_Link;


   overriding procedure Append_Attributes (Element : in out Html_Anchor) is
      Renderer : constant Renderer_Refs.Mutator := Element.Renderer.Update;
   begin
      Append_Attributes (Html_Element (Element));

      if not Element.Link.Is_Null then
         Renderer.Append_Attribute
           ("href", Slices.To_String (Element.Link), Allow_Empty => True);
      end if;

      if not Element.Title.Is_Null then
         Renderer.Append_Attribute
           ("title", Slices.To_String (Element.Title), Allow_Empty => True);
      end if;
   end Append_Attributes;



   -----------------
   -- Image token --
   -----------------

   overriding procedure Open (Element : in out Html_Image) is
   begin
      Open (Html_Element (Element));

      Append (Element.Renderer.Update.Data.Output, " alt=""");
   end Open;


   overriding procedure Close (Element : in out Html_Image) is
   begin
      Append (Element.Renderer.Update.Data.Output, """");

      Close (Html_Element (Element));
   end Close;


   overriding procedure Append
     (Element : in out Html_Image;
      Text : in String) is
   begin
      if not Element.Opened then
         raise Program_Error with "Append text to a non-opened token";
      end if;

      Element.Renderer.Update.Data.Append_Text (Text, Escape_Quotes => True);
   end Append;


   procedure Set_Height
     (Element : in out Html_Image;
      Height : in Integer) is
   begin
      if Height > 0 then
         Element.Height := Height;
      else
         Element.Height := 0;
      end if;
   end Set_Height;


   procedure Set_Size
     (Element : in out Html_Image;
      Width : in Integer;
      Height : in Integer) is
   begin
      Set_Width (Element, Width);
      Set_Height (Element, Height);
   end Set_Size;


   procedure Set_Width
     (Element : in out Html_Image;
      Width : in Integer) is
   begin
      if Width > 0 then
         Element.Width := Width;
      else
         Element.Width := 0;
      end if;
   end Set_Width;


   overriding procedure Append_Attributes (Element : in out Html_Image) is
      Renderer : constant Renderer_Refs.Mutator := Element.Renderer.Update;
   begin
      Append_Attributes (Html_Anchor (Element));

      if Element.Width > 0 then
         Renderer.Append_Attribute
           ("width", Positive'Image (Element.Width));
      end if;

      if Element.Height > 0 then
         Renderer.Append_Attribute
           ("height", Positive'Image (Element.Height));
      end if;
   end Append_Attributes;



   --------------------------
   -- Double-element token --
   --------------------------

   overriding procedure Open (Element : in out Html_Double_Element) is
      Newline_After : constant Boolean := Element.Newline_After_Open;
      Renderer : constant Renderer_Refs.Mutator := Element.Renderer.Update;
   begin
      Element.Newline_After_Open := False;
      Open (Html_Element (Element));
      Element.Newline_After_Open := Newline_After;

      Append
        (Renderer.Output,
         "<" & Element.Second_Tag (1 .. Positive (Element.Second_Size)) & ">");

      if Element.Newline_After_Open then
         Renderer.Append_Newline;
      end if;
   end Open;


   overriding procedure Close (Element : in out Html_Double_Element) is
      Newline_Before : constant Boolean := Element.Newline_Before_Close;
      Renderer : constant Renderer_Refs.Mutator := Element.Renderer.Update;
   begin
      if Element.Newline_Before_Close
        and then not Renderer.Beginning_Of_Line
      then
         Renderer.Append_Newline;
      end if;

      Renderer.Append_Indent;

      Append
        (Renderer.Output,
         "</"
         & Element.Second_Tag (1 .. Positive (Element.Second_Size))
         & ">");

      Element.Newline_Before_Close := False;
      Close (Html_Element (Element));
      Element.Newline_Before_Close := Newline_Before;
   end Close;



   --------------------
   -- Raw data token --
   --------------------

   overriding procedure Close (Element : in out Raw_Data) is
   begin
      if Element.Newline_After then
         Element.Renderer.Update.Data.Append_Newline;
      end if;
   end Close;


   overriding procedure Append
     (Element : in out Raw_Data;
      Text : in String)
   is
      Renderer : constant Renderer_Refs.Mutator := Element.Renderer.Update;
   begin
      if Element.Escape then
         Renderer.Append_Text (Text);
      else
         Append (Renderer.Output, Text);
         Renderer.Beginning_Of_Line := False;
      end if;
   end Append;



   -----------------
   -- Table token --
   -----------------

   overriding procedure Open (Element : in out Html_Table) is
      Renderer : constant Renderer_Refs.Mutator := Element.Renderer.Update;
   begin
      Open (Html_Element (Element));

      if Element.Alignments.Is_Empty then
         return;
      end if;

      declare
         Align : constant Alignment_Array_Refs.Accessor
           := Element.Alignments.Query;
      begin
         for I in Align.Data'Range loop
            Renderer.Append_Pending_Newline;
            Renderer.Append_Indent;
            Append (Renderer.Output, "<col");
            Renderer.Append_Attribute
              ("style", Image (Align.Data (I)));

            case Renderer.Format is
               when Html =>
                  Append (Renderer.Output, ">");
               when Xhtml =>
                  Append (Renderer.Output, " />");
            end case;
            Renderer.Newline_Pending := True;
         end loop;
      end;
   end Open;


   overriding procedure Set_Alignment_List
     (Element : in out Html_Table;
      List : in Alignment_Array)
   is
      function Save_List return Alignment_Array;

      function Save_List return Alignment_Array is
      begin
         return List;
      end Save_List;

      Has_Explicit : Boolean := False;
   begin
      for I in List'Range loop
         if List (I) /= Default_Align then
            Has_Explicit := True;
            exit;
         end if;
      end loop;

      if Has_Explicit then
         Element.Alignments.Replace (Save_List'Access);
      else
         Element.Alignments.Reset;
      end if;
   end Set_Alignment_List;



   ----------------------------
   -- Block token generators --
   ----------------------------

   function Code_Block (Renderer : Renderer_Ref)
     return Element_Callback'Class is
   begin
      return Create (Renderer, "pre", "code",
         Newline_Before_Open => True,
         Newline_After_Close => True);
   end Code_Block;

   function Definition_Description (Renderer : Renderer_Ref)
     return Element_Callback'Class is
   begin
      return Create (Renderer, "dd",
         Newline_Before_Open => True,
         Newline_After_Close => True);
   end Definition_Description;

   function Definition_List (Renderer : Renderer_Ref)
     return Element_Callback'Class is
   begin
      return Create (Renderer, "dl",
        Newline_Before_Open => True,
        Newline_After_Open => True,
        Newline_Before_Close => True,
        Newline_After_Close => True);
   end Definition_List;

   function Definition_Title (Renderer : Renderer_Ref)
     return Element_Callback'Class is
   begin
      return Create (Renderer, "dt",
         Newline_Before_Open => True,
         Newline_After_Close => True);
   end Definition_Title;


   function Division (Renderer : Renderer_Ref) return Element_Callback'Class is
   begin
      return Create (Renderer, "div",
        Newline_Before_Open => True,
        Newline_After_Open => True,
        Newline_Before_Close => True,
        Newline_After_Close => True);
   end Division;


   function Header (Renderer : Renderer_Ref) return Element_Callback'Class is
   begin
      return Html_Header'(Create (Renderer, "h1",
         Newline_Before_Open => True,
         Newline_After_Close => True)
        with null record);
   end Header;


   function Horizontal_Rule (Renderer : Renderer_Ref)
     return Element_Callback'Class is
   begin
      return Create (Renderer, "hr",
        Newline_Before_Open => True,
        Newline_After_Close => True,
        Self_Closing => True);
   end Horizontal_Rule;


   function List_Item (Renderer : Renderer_Ref)
     return Element_Callback'Class is
   begin
      return Create (Renderer, "li",
        Newline_Before_Open => True,
        Newline_After_Close => True);
   end List_Item;


   function Ordered_List (Renderer : Renderer_Ref)
     return Element_Callback'Class is
   begin
      return Create (Renderer, "ol",
        Newline_Before_Open => True,
        Newline_After_Open => True,
        Newline_Before_Close => True,
        Newline_After_Close => True);
   end Ordered_List;


   function Paragraph (Renderer : Renderer_Ref)
     return Element_Callback'Class is
   begin
      return Create (Renderer, "p",
        Newline_Before_Open => True,
        Newline_After_Close => True);
   end Paragraph;


   function Quote_Block (Renderer : Renderer_Ref)
     return Element_Callback'Class is
   begin
      return Create (Renderer, "blockquote",
        Newline_Before_Open => True,
        Newline_After_Open => True,
        Newline_Before_Close => True,
        Newline_After_Close => True);
   end Quote_Block;


   function Raw_Html_Block (Renderer : Renderer_Ref)
     return Element_Callback'Class is
   begin
      return Raw_Data'(Renderer => Renderer,
                       Escape => False,
                       Newline_After => True);
   end Raw_Html_Block;


   function Table (Renderer : Renderer_Ref) return Element_Callback'Class is
   begin
      return Html_Table'(Create (Renderer, "table",
        Newline_Before_Open => True,
        Newline_After_Open => True,
        Newline_Before_Close => True,
        Newline_After_Close => True) with Alignments => <>);
   end Table;


   function Table_Data_Cell (Renderer : Renderer_Ref)
     return Element_Callback'Class is
   begin
      return Create (Renderer, "td",
         Newline_Before_Open => True,
         Newline_After_Close => True);
   end Table_Data_Cell;


   function Table_Header_Cell (Renderer : Renderer_Ref)
     return Element_Callback'Class is
   begin
      return Create (Renderer, "th",
         Newline_Before_Open => True,
         Newline_After_Close => True);
   end Table_Header_Cell;


   function Table_Row (Renderer : Renderer_Ref)
     return Element_Callback'Class is
   begin
      return Create (Renderer, "tr",
        Newline_Before_Open => True,
        Newline_After_Open => True,
        Newline_Before_Close => True,
        Newline_After_Close => True);
   end Table_Row;


   function Unordered_List (Renderer : Renderer_Ref)
     return Element_Callback'Class is
   begin
      return Create (Renderer, "ul",
        Newline_Before_Open => True,
        Newline_After_Open => True,
        Newline_Before_Close => True,
        Newline_After_Close => True);
   end Unordered_List;



   ---------------------------
   -- Span token generators --
   ---------------------------

   function Abbreviation (Renderer : Renderer_Ref)
     return Element_Callback'Class is
   begin
      return Html_Titled'(Create (Renderer, "abbr") with Title => <>);
   end Abbreviation;


   function Anchor (Renderer : Renderer_Ref) return Element_Callback'Class is
   begin
      return Html_Anchor'(Create (Renderer, "a") with Link | Title => <>);
   end Anchor;


   function Code_Span (Renderer : Renderer_Ref)
     return Element_Callback'Class is
   begin
      return Create (Renderer, "code");
   end Code_Span;


   function Emphasis (Renderer : Renderer_Ref) return Element_Callback'Class is
   begin
      return Create (Renderer, "em");
   end Emphasis;


   function Image (Renderer : Renderer_Ref) return Element_Callback'Class is
   begin
      return Html_Image'(Create (Renderer, "img", Self_Closing => True) with
        Link | Title => <>,
        Width | Height => 0);
   end Image;


   function Line_Break (Renderer : Renderer_Ref)
     return Element_Callback'Class is
   begin
      return Create (Renderer, "br",
        Newline_After_Close => True,
        Self_Closing => True);
   end Line_Break;


   function Quote_Span (Renderer : Renderer_Ref)
     return Element_Callback'Class is
   begin
      return Create (Renderer, "q");
   end Quote_Span;


   function Raw_Html_Span (Renderer : Renderer_Ref)
     return Element_Callback'Class is
   begin
      return Raw_Data'(Renderer => Renderer,
                       Escape => False,
                       Newline_After => False);
   end Raw_Html_Span;


   function Raw_Text_Span (Renderer : Renderer_Ref)
     return Element_Callback'Class is
   begin
      return Raw_Data'(Renderer => Renderer,
                       Escape => True,
                       Newline_After => False);
   end Raw_Text_Span;


   function Span (Renderer : Renderer_Ref) return Element_Callback'Class is
   begin
      return Create (Renderer, "span");
   end Span;


   function Strong (Renderer : Renderer_Ref) return Element_Callback'Class is
   begin
      return Create (Renderer, "strong");
   end Strong;


   function Strong_Emphasis (Renderer : Renderer_Ref)
     return Element_Callback'Class is
   begin
      return Create (Renderer, "strong", "em");
   end Strong_Emphasis;

end Markup.Renderers.Html;
