------------------------------------------------------------------------------
-- Copyright (c) 2013, Natacha Porté                                        --
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

private with Natools.References;
private with Ada.Strings.Unbounded;

generic
   type Accumulator is private;
   with procedure Append (To : in out Accumulator; Text : in String) is <>;

package Markup.Renderers.Html is
   pragma Preelaborate (Html);

   ----------------------------------
   -- HTML renderer internal state --
   ----------------------------------

   type Renderer_Ref is tagged private;

   type Output_Format is (Html, Xhtml);
   type Newline_Format is (Autodetect, CR, LF, CR_LF, LF_CR);


   function Get_Format (Renderer : Renderer_Ref) return Output_Format;

   procedure Set_Format (Renderer : in out Renderer_Ref;
                         Format : Output_Format);


   function Get_Newline (Renderer : Renderer_Ref) return Newline_Format;

   procedure Set_Newline (Renderer : in out Renderer_Ref;
                          Format : Newline_Format);


   function Get_Output (Renderer : Renderer_Ref) return Accumulator;

   procedure Set_Output (Renderer : in out Renderer_Ref;
                         Output : in Accumulator);

   ----------------------
   -- Element generators --
   ----------------------

   function Code_Block (Renderer : Renderer_Ref) return Element_Callback'Class;

   function Definition_Description (Renderer : Renderer_Ref)
     return Element_Callback'Class;

   function Definition_List (Renderer : Renderer_Ref)
     return Element_Callback'Class;

   function Definition_Title (Renderer : Renderer_Ref)
     return Element_Callback'Class;

   function Header (Renderer : Renderer_Ref) return Element_Callback'Class;

   function Horizontal_Rule (Renderer : Renderer_Ref)
     return Element_Callback'Class;

   function List_Item (Renderer : Renderer_Ref) return Element_Callback'Class;

   function Ordered_List (Renderer : Renderer_Ref)
     return Element_Callback'Class;

   function Paragraph (Renderer : Renderer_Ref) return Element_Callback'Class;

   function Quote_Block (Renderer : Renderer_Ref)
     return Element_Callback'Class;

   function Raw_Html_Block (Renderer : Renderer_Ref)
     return Element_Callback'Class;

   function Table (Renderer : Renderer_Ref) return Element_Callback'Class;

   function Table_Data_Cell (Renderer : Renderer_Ref)
     return Element_Callback'Class;

   function Table_Header_Cell (Renderer : Renderer_Ref)
     return Element_Callback'Class;

   function Table_Row (Renderer : Renderer_Ref) return Element_Callback'Class;

   function Unordered_List (Renderer : Renderer_Ref)
     return Element_Callback'Class;


   function Anchor (Renderer : Renderer_Ref) return Element_Callback'Class;

   function Code_Span (Renderer : Renderer_Ref) return Element_Callback'Class;

   function Emphasis (Renderer : Renderer_Ref) return Element_Callback'Class;

   function Image (Renderer : Renderer_Ref) return Element_Callback'Class;

   function Line_Break (Renderer : Renderer_Ref) return Element_Callback'Class;

   function Quote_Span (Renderer : Renderer_Ref) return Element_Callback'Class;

   function Raw_Html_Span (Renderer : Renderer_Ref)
     return Element_Callback'Class;

   function Raw_Text_Span (Renderer : Renderer_Ref)
     return Element_Callback'Class;

   function Span (Renderer : Renderer_Ref) return Element_Callback'Class;

   function Strong (Renderer : Renderer_Ref) return Element_Callback'Class;

   function Strong_Emphasis (Renderer : Renderer_Ref)
     return Element_Callback'Class;


private

   type Renderer_Data is tagged record
      Output : Accumulator;
      Format : Output_Format := Html;
      Newline : Newline_Format := Autodetect;
      Autodetected : Newline_Format := Autodetect;
      Newline_Pending : Boolean := False;
      Indent_Level : Natural := 0;
   end record;

   procedure Append_Attribute
     (State : in out Renderer_Data;
      Name : in String;
      Value : in String;
      Allow_Empty : in Boolean := False);
   --  Append the given attribute,
   --  or do nothing when Allow_Empty is true and value is empty string

   procedure Append_Text
     (State : in out Renderer_Data;
      Text : in String;
      Escape_Quotes : in Boolean := False);
   --  Escape the given text and output it

   procedure Append_Newline
     (State : in out Renderer_Data;
      Indent : in Boolean := False);
   --  Output a newline if format is known

   procedure Append_Pending_Newline (State : in out Renderer_Data);
   --  Output a newline if pending and format is known

   procedure Update_Indent
     (State : in out Renderer_Data;
      Level_Change : in Integer);
   --  Change indent level by the given amount


   type Dummy_Access is access Boolean;
   --  Used to provide default storage pool to Natools.References

   package Renderer_Refs is new Natools.References
     (Renderer_Data, Dummy_Access'Storage_Pool, Dummy_Access'Storage_Pool);

   type Renderer_Ref is new Renderer_Refs.Reference with null record;


   -----------------
   -- Base tokens --
   -----------------

   Html_Tag_Max_Size : constant := 10;

   type Html_Tag_Size is range 1 .. Html_Tag_Max_Size;

   type Html_Element is new Element_Callback and With_Identity with record
      Tag : String (1 .. Html_Tag_Max_Size);
      Tag_Size : Html_Tag_Size;
      Newline_Before_Open : Boolean;
      Newline_After_Open : Boolean;
      Newline_Before_Close : Boolean;
      Newline_After_Close : Boolean;
      Self_Closing : Boolean;
      Classes : Ada.Strings.Unbounded.Unbounded_String;
      Id : Natools.String_Slices.Slice;
      Opened : Boolean;
      Renderer : Renderer_Ref;
   end record;

   overriding procedure Open (Element : in out Html_Element);
   overriding procedure Close (Element : in out Html_Element);
   overriding procedure Append
     (Element : in out Html_Element;
      Text : in String);

   overriding procedure Add_Class
     (Element : in out Html_Element;
      Class : in Natools.String_Slices.Slice);
   overriding procedure Reset_Class_List
     (Element : in out Html_Element);
   overriding procedure Set_Id
     (Element : in out Html_Element;
      Name : in Natools.String_Slices.Slice);

   procedure Append_Attributes (Element : in out Html_Element);


   ------------------------
   -- Specialized tokens --
   ------------------------

   type Html_Header is new Html_Element and With_Level with null record;

   overriding procedure Set_Level
     (Element : in out Html_Header;
      Level : in Positive);


   type Html_Anchor is new Html_Element and With_Link and With_Title
     with record
      Link, Title : Natools.String_Slices.Slice;
   end record;

   overriding procedure Set_Link
     (Element : in out Html_Anchor;
      Link : in Natools.String_Slices.Slice);

   overriding procedure Set_Title
     (Element : in out Html_Anchor;
      Title : in Natools.String_Slices.Slice);

   overriding procedure Append_Attributes (Element : in out Html_Anchor);


   type Html_Image is new Html_Anchor and With_Size with record
      Width, Height : Natural := 0;
   end record;

   overriding procedure Open (Element : in out Html_Image);
   overriding procedure Close (Element : in out Html_Image);
   overriding procedure Append
     (Element : in out Html_Image;
      Text : in String);

   procedure Set_Height
     (Element : in out Html_Image;
      Height : in Integer);
   procedure Set_Size
     (Element : in out Html_Image;
      Width : in Integer;
      Height : in Integer);
   procedure Set_Width
     (Element : in out Html_Image;
      Width : in Integer);

   overriding procedure Append_Attributes (Element : in out Html_Image);


   type Html_Double_Element is new Html_Element with record
      Second_Tag : String (1 .. Html_Tag_Max_Size);
      Second_Size : Html_Tag_Size;
   end record;

   overriding procedure Open (Element : in out Html_Double_Element);
   overriding procedure Close (Element : in out Html_Double_Element);


   type Raw_Data is new Element_Callback with record
      Renderer : Renderer_Ref;
      Escape : Boolean := True;
      Newline_After : Boolean := False;
   end record;

   overriding procedure Open (Element : in out Raw_Data) is null;
   overriding procedure Close (Element : in out Raw_Data);
   overriding procedure Append
     (Element : in out Raw_Data;
      Text : in String);

end Markup.Renderers.Html;
