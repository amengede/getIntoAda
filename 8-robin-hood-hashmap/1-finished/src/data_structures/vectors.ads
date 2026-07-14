with Ada.Finalization;

generic
   type T is private;
package Vectors is

   Out_Of_Bounds_Exception : exception;
   Uninitialized_Exception : exception;

   type Vector is tagged private;
   
   procedure Push_Back (Self: in out Vector; Element: T);

   function Get (Self: Vector; Index: Natural) return T;

   procedure Reserve (Self : in out Vector; New_Capacity : Natural);

   procedure Resize (Self : in out Vector; New_Size : Natural);

   procedure Erase (Self : in out Vector; Index : Natural);

   procedure Clear (Self : in out Vector);

   procedure Shrink_To_Fit (Self : in out Vector);

   function Get_Capacity (Self : in out Vector) return Natural;

   function Get_Size (Self : in out Vector) return Natural;

private
   type Elements_Type is array (Natural range <>) of T;
   type Elements_Access is access Elements_Type;

   type Internal_Vector is
      record
         Elements: Elements_Access := new Elements_Type (0 .. 1);
         Size: Natural := 0;
         Capacity: Natural := 1;
         Reference_Count : Natural := 1;
      end record;

   type Internal_Vector_Ptr is access Internal_Vector;

   type Vector is new Ada.Finalization.Controlled with
      record
         Data: Internal_Vector_Ptr := null;
      end record;

   overriding
   procedure Adjust (Self : in out Vector);

   overriding
   procedure Finalize (Self : in out Vector);
end Vectors;
