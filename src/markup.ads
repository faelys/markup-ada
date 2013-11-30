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

with Natools.String_Slices;

package Markup is
   pragma Preelaborate (Markup);

   type Element_Callback is interface;

   procedure Open (Element : in out Element_Callback) is abstract;
   procedure Append
     (Element : in out Element_Callback;
      Text  : in String) is abstract;
   procedure Close (Element : in out Element_Callback) is abstract;


   type With_Identity is interface;
   procedure Add_Class
     (Element : in out With_Identity;
      Class : in Natools.String_Slices.Slice) is null;
   procedure Reset_Class_List
     (Element : in out With_Identity) is null;
   procedure Set_Id
     (Element : in out With_Identity;
      Name : in Natools.String_Slices.Slice) is abstract;


   type With_Level is interface;
   procedure Set_Level
     (Element : in out With_Level;
      Level : in Positive) is abstract;


   type With_Link is interface;
   procedure Set_Link
     (Element : in out With_Link;
      Link  : in Natools.String_Slices.Slice) is abstract;


   type With_Size is interface;
   procedure Set_Height
     (Element : in out With_Size;
      Height : in Integer) is abstract;
   procedure Set_Size
     (Element : in out With_Size;
      Width : in Integer;
      Height : in Integer) is abstract;
   procedure Set_Width
     (Element : in out With_Size;
      Width : in Integer) is abstract;


   type With_Title is interface;
   procedure Set_Title
     (Element : in out With_Title;
      Title : in Natools.String_Slices.Slice) is abstract;

   type Alignment is
     (Default_Align, Left_Aligned, Centered_Text, Right_Aligned);
   type With_Alignment is interface;

   procedure Set_Alignment
     (Element : in out With_Alignment;
      Align : in Alignment) is abstract;

end Markup;
