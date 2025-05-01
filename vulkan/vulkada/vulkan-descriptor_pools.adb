-- This file is part of VulkAda.

-- VulkAda is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Lesser General Public License as
-- published by the Free Software Foundation, either version 3 of
-- the License, or (at your option) any later version.

-- VulkAda is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Lesser General Public License for more details.

-- You should have received a copy of the GNU Lesser General Public
-- License along with VulkAda.
-- If not, see <http://www.gnu.org/licenses/>.

-- Copyright 2024 Phaser Cat Games LLC

-- Descriptor pool related subprograms

with Ada.Unchecked_Conversion;
with Vulkan.Exceptions;
with Vulkan.C_V1_1;

package body Vulkan.Descriptor_Pools is
    procedure Reset(Device: in Vulkan.Device;
                    Descriptor_Pool: in Vulkan.Descriptor_Pool) is
        Result: Vulkan.Result;
    begin
        Result := C.vkResetDescriptorPool(Device, Descriptor_Pool, 0);
    end Reset;

    function Allocate(Device: in Vulkan.Device;
                      Allocate_Info: in Descriptor_Set_Allocate_Info;
                      Descriptor_Sets: out Descriptor_Set_Vectors.Vector) return Result is
        Allocate_Info_C: C.Descriptor_Set_Allocate_Info_C := C.To_C(Allocate_Info);
        Descriptor_Sets_C: array (1 .. Positive(Allocate_Info.Set_Layouts.Length))
                                of aliased Descriptor_Set
            with Convention => C;
        Result: Vulkan.Result;
    begin
        Result := C.vkAllocateDescriptorSets(Device,
                                             Allocate_Info_C,
                                             Descriptor_Sets_C(1)'Access);
        C.Free(Allocate_Info_C);

        if Result = Success then
            Descriptor_Sets.Set_Length(Allocate_Info.Set_Layouts.Length);

            for X in Descriptor_Sets_C'Range loop
                Descriptor_Sets(X) := Descriptor_Sets_C(X);
            end loop;
        end if;

        return Result;
    end Allocate;

    function Allocate(Device: in Vulkan.Device;
                      Allocate_Info: in Descriptor_Set_Allocate_Info;
                      Descriptor_Set: out Vulkan.Descriptor_Set) return Result is
        Descriptor_Sets: Descriptor_Set_Vectors.Vector;
        Result: Vulkan.Result;
    begin
        Result := Allocate(Device, Allocate_Info, Descriptor_Sets);

        if not Descriptor_Sets.Is_Empty then
            Descriptor_Set := Descriptor_Sets.First_Element;
        end if;

        return Result;
    end Allocate;

    function Allocate(Device: in Vulkan.Device;
                      Allocate_Info: in Descriptor_Set_Allocate_Info)
                        return Descriptor_Set_Vectors.Vector is
        Descriptor_Sets: Descriptor_Set_Vectors.Vector;
    begin
        Exceptions.Check(Allocate(Device, Allocate_Info, Descriptor_Sets));

        return Descriptor_Sets;
    end Allocate;

    function Allocate(Device: in Vulkan.Device;
                      Allocate_Info: in Descriptor_Set_Allocate_Info)
                        return Descriptor_Set is
        Descriptor_Set: Vulkan.Descriptor_Set;
    begin
        Exceptions.Check(Allocate(Device, Allocate_Info, Descriptor_Set));

        return Descriptor_Set;
    end Allocate;

    procedure Free(Device: in Vulkan.Device;
                   Descriptor_Pool: in Vulkan.Descriptor_Pool;
                   Descriptor_Sets: in out Descriptor_Set_Vectors.Vector) is
        Descriptor_Sets_C: array (1 .. Natural(Descriptor_Sets.Length))
                            of aliased Descriptor_Set
            with Convention => C;
        Result: Vulkan.Result;
    begin
        if Descriptor_Sets.Is_Empty then
            return;
        end if;

        for X in Descriptor_Sets_C'Range loop
            Descriptor_Sets_C(X) := Descriptor_Sets(X);
        end loop;

        Result := C.vkFreeDescriptorSets(Device,
                                         Descriptor_Pool,
                                         Interfaces.Unsigned_32(Descriptor_Sets.Length),
                                         Descriptor_Sets_C(1)'Access);
        Descriptor_Sets.Clear;
    end Free;

    procedure Free(Device: in Vulkan.Device;
                   Descriptor_Pool: in Vulkan.Descriptor_Pool;
                   Descriptor_Set: in out Vulkan.Descriptor_Set) is
        Descriptor_Sets: Descriptor_Set_Vectors.Vector;
    begin
        Descriptor_Sets.Append(Descriptor_Set);

        Free(Device, Descriptor_Pool, Descriptor_Sets);
        Descriptor_Set := No_Descriptor_Set;
    end Free;

    procedure Update(Device: in Vulkan.Device;
                     Descriptor_Writes: in Write_Descriptor_Set_Vectors.Vector
                        := Write_Descriptor_Set_Vectors.Empty_Vector;
                     Descriptor_Copies: in Copy_Descriptor_Set_Vectors.Vector
                        := Copy_Descriptor_Set_Vectors.Empty_Vector) is
        C_Writes: array (1 .. Natural(Descriptor_Writes.Length))
                    of aliased C.Write_Descriptor_Set_C
            with Convention => C;
        C_Copies: array (1 .. Natural(Descriptor_Copies.Length))
                    of aliased C.Copy_Descriptor_Set_C
            with Convention => C;
    begin
        for X in C_Writes'Range loop
            C_Writes(X) := C.To_C(Descriptor_Writes(X));
        end loop;

        for X in C_Copies'Range loop
            C_Copies(X) := C.To_C(Descriptor_Copies(X));
        end loop;

        C.vkUpdateDescriptorSets
            (Device,
             Interfaces.Unsigned_32(Descriptor_Writes.Length),
             (if C_Writes'Length /= 0 then C_Writes(1)'Access else null),
             Interfaces.Unsigned_32(Descriptor_Copies.Length),
             (if C_Copies'Length /= 0 then C_Copies(1)'Access else null));

        for Write of C_Writes loop
            C.Free(Write);
        end loop;

        for Copy of C_Copies loop
            C.Free(Copy);
        end loop;
    end Update;

    procedure Update_With_Template
        (Device: in Vulkan.Device;
         Descriptor_Set: in Vulkan.Descriptor_Set;
         Descriptor_Update_Template: in Vulkan.Descriptor_Update_Template;
         Data: in Pointers.Pointer) is
        function To_Pointer is
            new Ada.Unchecked_Conversion(Pointers.Pointer,
                                         Interfaces.C.Extensions.void_ptr);
    begin
        C_V1_1.vkUpdateDescriptorSetWithTemplate(Device,
                                                 Descriptor_Set,
                                                 Descriptor_Update_Template,
                                                 To_Pointer(Data));
    end Update_With_Template;
end Vulkan.Descriptor_Pools;

