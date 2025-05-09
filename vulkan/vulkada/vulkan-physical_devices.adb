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

-- Copyright 2025 Phaser Cat Games LLC

-- Physical device routines

with Interfaces.C.Strings;
with Vulkan.Exceptions;
with Vulkan.Utilities;
with Vulkan.Extension_Records;
with Vulkan.C_V1_1;
with Vulkan.C_V1_3;

package body Vulkan.Physical_Devices is
    -- Common code for Enumerate_Extension_Properties.
    function Enumerate_Extension_Properties
        (Device: in Physical_Device;
         Layer_Name: in Interfaces.C.Strings.char_array_access)
        return Extension_Properties_Vectors.Vector;

    function Get_Features(Device: in Physical_Device)
        return Physical_Device_Features is
        PDFC: C.Physical_Device_Features_C;
    begin
        C.vkGetPhysicalDeviceFeatures(Device, PDFC);

        return Utilities.To_Ada(PDFC);
    end Get_Features;

    function Get_Format_Properties
        (Device: in Physical_Device;
         Format: in Vulkan.Format) return Format_Properties is
        Properties: Format_Properties;
    begin
        C.vkGetPhysicalDeviceFormatProperties(Device, Format, Properties);

        return Properties;
    end Get_Format_Properties;

    function Get_Image_Format_Properties
        (Device: in Physical_Device;
         Format: in Vulkan.Format;
         Image_Type: in Vulkan.Image_Type;
         Tiling: in Image_Tiling;
         Usage: in Image_Usage_Flags;
         Create_Flags: in Image_Create_Flags;
         Properties: out Image_Format_Properties) return Result is
    begin
        return C.vkGetPhysicalDeviceImageFormatProperties(Device,
                                                          Format,
                                                          Image_Type,
                                                          Tiling,
                                                          Usage,
                                                          Create_Flags,
                                                          Properties);
    end Get_Image_Format_Properties;

    function Get_Image_Format_Properties(Device: in Physical_Device;
                                         Format: in Vulkan.Format;
                                         Image_Type: in Vulkan.Image_Type;
                                         Tiling: in Image_Tiling;
                                         Usage: in Image_Usage_Flags;
                                         Create_Flags: in Image_Create_Flags)
        return Image_Format_Properties is
        Properties: Image_Format_Properties;
    begin
        Exceptions.Check(Get_Image_Format_Properties(Device,
                                                     Format,
                                                     Image_Type,
                                                     Tiling,
                                                     Usage,
                                                     Create_Flags,
                                                     Properties));

        return Properties;
    end Get_Image_Format_Properties;

    function Get_Properties(Device: in Physical_Device)
        return Physical_Device_Properties is
        PDPC: C.Physical_Device_Properties_C;
    begin
        C.vkGetPhysicalDeviceProperties(Device, PDPC);

        return Utilities.To_Ada(PDPC);
    end Get_Properties;

    function Get_Memory_Properties(Device: in Physical_Device)
        return Physical_Device_Memory_Properties is
        PDMPC: C.Physical_Device_Memory_Properties_C;
    begin
        C.vkGetPhysicalDeviceMemoryProperties(Device, PDMPC);

        return Utilities.To_Ada(PDMPC);
    end Get_Memory_Properties;

    function Enumerate_Extension_Properties(Device: in Physical_Device;
                                            Layer_Name: in String)
        return Extension_Properties_Vectors.Vector is
        C_Layer_Name: aliased Interfaces.C.char_array :=
            Interfaces.C.To_C(Layer_Name);
    begin
        return Enumerate_Extension_Properties(Device,
                                              C_Layer_Name'Unchecked_Access);
    end Enumerate_Extension_Properties;

    function Enumerate_Extension_Properties(Device: in Physical_Device)
        return Extension_Properties_Vectors.Vector is
    begin
        return Enumerate_Extension_Properties(Device, null);
    end Enumerate_Extension_Properties;
 
    function Enumerate_Layer_Properties(Device: in Physical_Device)
        return Layer_Properties_Vectors.Vector is
        use type Interfaces.Unsigned_32;

        Count: Interfaces.Unsigned_32 := 0;
    begin
        if C.vkEnumerateDeviceLayerProperties(Device,
                                              Count,
                                              null) /= Success then
            return Layer_Properties_Vectors.Empty_Vector;
        end if;

        if Count = 0 then
            return Layer_Properties_Vectors.Empty_Vector;
        end if;

        declare
            Properties: Utilities.C_Layer_Properties_Array
                            (1 .. Positive(Count));
        begin
            if C.vkEnumerateDeviceLayerProperties
                (Device,
                 Count,
                 Properties(1)'Access) /= Success then
                return Layer_Properties_Vectors.Empty_Vector;
            end if;

            return Utilities.To_Ada(Properties);
        end;
    end Enumerate_Layer_Properties;
    
    function Get_Sparse_Image_Format_Properties
        (Device: in Physical_Device;
         Format: in Vulkan.Format;
         Image_Type: in Vulkan.Image_Type;
         Samples: in Sample_Count_Flags;
         Usage: in Image_Usage_Flags;
         Tiling: in Image_Tiling)
        return Sparse_Image_Format_Properties_Vectors.Vector is
        use type Interfaces.Unsigned_32;

        Count: Interfaces.Unsigned_32 := 0;
    begin
        C.vkGetPhysicalDeviceSparseImageFormatProperties(Device,
                                                         Format,
                                                         Image_Type,
                                                         Samples,
                                                         Usage,
                                                         Tiling,
                                                         Count,
                                                         null);

        if Count = 0 then
            return Sparse_Image_Format_Properties_Vectors.Empty_Vector;
        end if;

        declare
            Properties_Array: array (1 .. Count)
                of aliased Sparse_Image_Format_Properties
                with Convention => C;
            Properties: Sparse_Image_Format_Properties_Vectors.Vector;
        begin
            Properties.Reserve_Capacity(Ada.Containers.Count_Type(Count));

            C.vkGetPhysicalDeviceSparseImageFormatProperties
                (Device,
                 Format,
                 Image_Type,
                 Samples,
                 Usage,
                 Tiling,
                 Count,
                 Properties_Array(1)'Access);

            for Property of Properties_Array loop
                Properties.Append(Property);
            end loop;

            return Properties;
        end;
    end Get_Sparse_Image_Format_Properties;

    function Group_Properties_Count(Instance: in Vulkan.Instance)
        return Interfaces.Unsigned_32 is
        Count: Interfaces.Unsigned_32 := 0;
    begin
        Exceptions.Check(C_V1_1.vkEnumeratePhysicalDeviceGroups(Instance,
                                                                Count,
                                                                null));

        return Count;
    end Group_Properties_Count;

    function Enumerate
        (Instance: in Vulkan.Instance;
         Properties: in out Physical_Device_Group_Properties_Vectors.Vector)
        return Result is
        Count: Interfaces.Unsigned_32 := 0;
        Result: Vulkan.Result;
    begin
        Result := C_V1_1.vkEnumeratePhysicalDeviceGroups(Instance,
                                                         Count,
                                                         null);

        if Result not in Success | Incomplete then
            return Result;
        end if;

        declare
            use type Interfaces.Unsigned_32;

            C_Properties: array (1 .. Count) of
                aliased C_V1_1.Physical_Device_Group_Properties_C
                with Convention => C;
            Index: Interfaces.Unsigned_32 := 1;
        begin
            for P of Properties loop
                C_Properties(Index).Next := Extension_Records.To_C(P.Next);
                Index := Index + 1;
            end loop;

            Result := C_V1_1.vkEnumeratePhysicalDeviceGroups
                (Instance, Count, C_Properties(1)'Unchecked_Access);

            if Result not in Success | Incomplete then
                for CP of C_Properties loop
                    Extension_Records.Free(CP.Next);
                end loop;

                return Result;
            end if;

            Index := 1;

            for P of Properties loop
                C_V1_1.To_Ada(P, C_Properties(Index));
                Extension_Records.Free(C_Properties(Index).Next);
                Index := Index + 1;
            end loop;
        end;
                                                        
        return Result;
    end Enumerate;

    function Enumerate(Instance: in Vulkan.Instance)
        return Physical_Device_Group_Properties_Vectors.Vector is
        Properties: Physical_Device_Group_Properties_Vectors.Vector;
        Item: Physical_Device_Group_Properties;
        Count: Interfaces.Unsigned_32 := 0;
    begin
        Count := Group_Properties_Count(Instance);
        Properties.Append(Item, Ada.Containers.Count_Type(Count));
        Exceptions.Check(Enumerate(Instance, Properties));
                                                        
        return Properties;
    end Enumerate;

    procedure Get_Features(Device: in Physical_Device;
                           Features: in out Physical_Device_Features_2) is
        C_Features: C_V1_1.Physical_Device_Features_2_C;
    begin
        C_Features.Next := Extension_Records.To_C(Features.Next);
        C_V1_1.vkGetPhysicalDeviceFeatures2(Device, C_Features);
        C_V1_1.To_Ada(Features, C_Features);
        Extension_Records.Free(C_Features.Next);
    end Get_Features;

    function Get_Features(Device: in Physical_Device)
        return Physical_Device_Features_2 is
        Features: Physical_Device_Features_2;
    begin
        Get_Features(Device, Features);

        return Features;
    end Get_Features;

    procedure Get_Properties(Device: in Physical_Device;
                             Properties: in out Physical_Device_Properties_2) is
        C_Properties: C_V1_1.Physical_Device_Properties_2_C;
    begin
        C_Properties.Next := Extension_Records.To_C(Properties.Next);
        C_V1_1.vkGetPhysicalDeviceProperties2(Device, C_Properties);
        C_V1_1.To_Ada(Properties, C_Properties);
        Extension_Records.Free(C_Properties.Next);
    end Get_Properties;

    function Get_Properties(Device: in Physical_Device)
        return Physical_Device_Properties_2 is
        Properties: Physical_Device_Properties_2;
    begin
        Get_Properties(Device, Properties);

        return Properties;
    end Get_Properties;

    procedure Get_Format_Properties(Device: in Physical_Device;
                                    Format: in Vulkan.Format;
                                    Properties: in out Format_Properties_2) is
        C_Properties: C_V1_1.Format_Properties_2_C;
    begin
        C_Properties.Next := Extension_Records.To_C(Properties.Next);
        C_V1_1.vkGetPhysicalDeviceFormatProperties2(Device,
                                                    Format,
                                                    C_Properties);
        C_V1_1.To_Ada(Properties, C_Properties);
        Extension_Records.Free(C_Properties.Next);
    end Get_Format_Properties;

    function Get_Format_Properties(Device: in Physical_Device;
                                   Format: in Vulkan.Format)
        return Format_Properties_2 is
        Properties: Format_Properties_2;
    begin
        Get_Format_Properties(Device, Format, Properties);

        return Properties;
    end Get_Format_Properties;

    function Get_Image_Format_Properties
        (Device: in Physical_Device;
         Image_Format_Info: in Physical_Device_Image_Format_Info_2;
         Image_Format_Properties: in out Image_Format_Properties_2)
        return Result is
        C_Format_Info: C_V1_1.Physical_Device_Image_Format_Info_2_C :=
            C_V1_1.To_C(Image_Format_Info);
        C_Properties: C_V1_1.Image_Format_Properties_2_C;
        Result: Vulkan.Result;
    begin
        C_Properties.Next :=
            Extension_Records.To_C(Image_Format_Properties.Next);
        Result := C_V1_1.vkGetPhysicalDeviceImageFormatProperties2
            (Device, C_Format_Info, C_Properties);
        C_V1_1.Free(C_Format_Info);
        C_V1_1.To_Ada(Image_Format_Properties, C_Properties);
        Extension_Records.Free(C_Properties.Next);

        return Result;
    end Get_Image_Format_Properties;

    function Get_Image_Format_Properties
        (Device: in Physical_Device;
         Image_Format_Info: in Physical_Device_Image_Format_Info_2)
        return Image_Format_Properties_2 is
        Properties: Image_Format_Properties_2;
    begin
        Exceptions.Check(Get_Image_Format_Properties(Device,
                                                     Image_Format_Info,
                                                     Properties));

        return Properties;
    end Get_Image_Format_Properties;

    function Queue_Family_Properties_Count(Device: in Physical_Device)
        return Interfaces.Unsigned_32 is
        Count: Interfaces.Unsigned_32 := 0;
    begin
        C_V1_1.vkGetPhysicalDeviceQueueFamilyProperties2(Device, Count, null);

        return Count;
    end Queue_Family_Properties_Count;

    procedure Get_Queue_Family_Properties
        (Device: in Physical_Device;
         Properties: in out Queue_Family_Properties_2_Vectors.Vector) is
        use type Interfaces.Unsigned_32;

        Count: Interfaces.Unsigned_32 := 0;
    begin
        C_V1_1.vkGetPhysicalDeviceQueueFamilyProperties2(Device, Count, null);

        if Count = 0 then
            return;
        end if;

        declare
            C_Properties: array (1 .. Count) of
                aliased C_V1_1.Queue_Family_Properties_2_C
                with Convention => C;
            A_Properties: Queue_Family_Properties_2;
            Index: Interfaces.Unsigned_32 := 1;
        begin
            for P of Properties loop
                C_Properties(Index).Next := Extension_Records.To_C(P.Next);
                Index := Index + 1;
            end loop;

            C_V1_1.vkGetPhysicalDeviceQueueFamilyProperties2
                (Device, Count, C_Properties(1)'Unchecked_Access);

            Index := 1;

            for P of Properties loop
                C_V1_1.To_Ada(P, C_Properties(Index));
                Extension_Records.Free(C_Properties(Index).Next);
                Index := Index + 1;
            end loop;
        end;
    end Get_Queue_Family_Properties;

    function Get_Queue_Family_Properties(Device: in Physical_Device)
        return Queue_Family_Properties_2_Vectors.Vector is
        Count: Interfaces.Unsigned_32 := 0;
        Item: Queue_Family_Properties_2;
        Properties: Queue_Family_Properties_2_Vectors.Vector;
    begin
        Count := Queue_Family_Properties_Count(Device);
        Properties.Append(Item, Ada.Containers.Count_Type(Count));
        Get_Queue_Family_Properties(Device, Properties);

        return Properties;
    end Get_Queue_Family_Properties;

    procedure Get_Memory_Properties
        (Device: in Physical_Device;
         Properties: in out Physical_Device_Memory_Properties_2) is
        C_Properties: C_V1_1.Physical_Device_Memory_Properties_2_C;
    begin
        C_Properties.Next := Extension_Records.To_C(Properties.Next);
        C_V1_1.vkGetPhysicalDeviceMemoryProperties2(Device, C_Properties);
        C_V1_1.To_Ada(Properties, C_Properties);
        Extension_Records.Free(C_Properties.Next);
    end Get_Memory_Properties;

    function Get_Memory_Properties(Device: in Physical_Device)
        return Physical_Device_Memory_Properties_2 is
        Properties: Physical_Device_Memory_Properties_2;
    begin
        Get_Memory_Properties(Device, Properties);

        return Properties;
    end Get_Memory_Properties;

    function Sparse_Image_Format_Properties_Count
        (Device: in Physical_Device;
         Format_Info: in Physical_Device_Sparse_Image_Format_Info_2)
        return Interfaces.Unsigned_32 is
        Count: Interfaces.Unsigned_32 := 0;
        C_Format_Info: C_V1_1.Physical_Device_Sparse_Image_Format_Info_2_C :=
            C_V1_1.To_C(Format_Info);
    begin
        C_V1_1.vkGetPhysicalDeviceSparseImageFormatProperties2
            (Device, C_Format_Info, Count, null);
        C_V1_1.Free(C_Format_Info);

        return Count;
    end Sparse_Image_Format_Properties_Count;

    procedure Get_Sparse_Image_Format_Properties
        (Device: in Physical_Device;
         Format_Info: in Physical_Device_Sparse_Image_Format_Info_2;
         Properties: in out Sparse_Image_Format_Properties_2_Vectors.Vector) is
        use type Interfaces.Unsigned_32;

        Count: Interfaces.Unsigned_32 := 0;
        C_Format_Info: C_V1_1.Physical_Device_Sparse_Image_Format_Info_2_C :=
            C_V1_1.To_C(Format_Info);
    begin
        C_V1_1.vkGetPhysicalDeviceSparseImageFormatProperties2
            (Device, C_Format_Info, Count, null);

        if Count = 0 then
            C_V1_1.Free(C_Format_Info);

            return;
        end if;

        declare
            C_Properties: array (1 .. Count) of
                aliased C_V1_1.Sparse_Image_Format_Properties_2_C
                with Convention => C;
            A_Properties: Sparse_Image_Format_Properties_2;
            Index: Interfaces.Unsigned_32 := 1;
        begin
            for P of Properties loop
                C_Properties(Index).Next := Extension_Records.To_C(P.Next);
                Index := Index + 1;
            end loop;

            C_V1_1.vkGetPhysicalDeviceSparseImageFormatProperties2
                (Device,
                 C_Format_Info,
                 Count,
                 C_Properties(1)'Unchecked_Access);

            Index := 1;

            for P of Properties loop
                C_V1_1.To_Ada(P, C_Properties(Index));
                Extension_Records.Free(C_Properties(Index).Next);
                Index := Index + 1;
            end loop;
        end;

        C_V1_1.Free(C_Format_Info);
    end Get_Sparse_Image_Format_Properties;

    function Get_Sparse_Image_Format_Properties
        (Device: in Physical_Device;
         Format_Info: in Physical_Device_Sparse_Image_Format_Info_2)
        return Sparse_Image_Format_Properties_2_Vectors.Vector is
        use type Interfaces.Unsigned_32;

        Count: Interfaces.Unsigned_32 := 0;
        Item: Sparse_Image_Format_Properties_2;
        Properties: Sparse_Image_Format_Properties_2_Vectors.Vector;
    begin
        Count := Sparse_Image_Format_Properties_Count(Device, Format_Info);
        Properties.Append(Item, Ada.Containers.Count_Type(Count));
        Get_Sparse_Image_Format_Properties(Device, Format_Info, Properties);

        return Properties;
    end Get_Sparse_Image_Format_Properties;

    procedure Get_External_Buffer_Properties
        (Device: in Physical_Device;
         Buffer_Info: in Physical_Device_External_Buffer_Info;
         Properties: in out External_Buffer_Properties) is
        C_Buffer_Info: C_V1_1.Physical_Device_External_Buffer_Info_C :=
            C_V1_1.To_C(Buffer_Info);
        C_Properties: C_V1_1.External_Buffer_Properties_C;
    begin
        C_Properties.Next := Extension_Records.To_C(Properties.Next);
        C_V1_1.vkGetPhysicalDeviceExternalBufferProperties(Device,
                                                           C_Buffer_Info,
                                                           C_Properties);
        C_V1_1.Free(C_Buffer_Info);
        C_V1_1.To_Ada(Properties, C_Properties);
        Extension_Records.Free(C_Properties.Next);
    end Get_External_Buffer_Properties;

    function Get_External_Buffer_Properties
        (Device: in Physical_Device;
         Buffer_Info: in Physical_Device_External_Buffer_Info)
        return External_Buffer_Properties is
        Properties: External_Buffer_Properties;
    begin
        Get_External_Buffer_Properties(Device, Buffer_Info, Properties);

        return Properties;
    end Get_External_Buffer_Properties;

    procedure Get_External_Fence_Properties
        (Device: in Physical_Device;
         Fence_Info: in Physical_Device_External_Fence_Info;
         Properties: in out External_Fence_Properties) is
        C_Fence_Info: C_V1_1.Physical_Device_External_Fence_Info_C :=
            C_V1_1.To_C(Fence_Info);
        C_Properties: C_V1_1.External_Fence_Properties_C;
    begin
        C_Properties.Next := Extension_Records.To_C(Properties.Next);
        C_V1_1.vkGetPhysicalDeviceExternalFenceProperties(Device,
                                                          C_Fence_Info,
                                                          C_Properties);
        C_V1_1.Free(C_Fence_Info);
        C_V1_1.To_Ada(Properties, C_Properties);
        Extension_Records.Free(C_Properties.Next);
    end Get_External_Fence_Properties;

    function Get_External_Fence_Properties
        (Device: in Physical_Device;
         Fence_Info: in Physical_Device_External_Fence_Info)
        return External_Fence_Properties is
        Properties: External_Fence_Properties;
    begin
        Get_External_Fence_Properties(Device, Fence_Info, Properties);

        return Properties;
    end Get_External_Fence_Properties;

    procedure Get_External_Semaphore_Properties
        (Device: in Physical_Device;
         Semaphore_Info: in Physical_Device_External_Semaphore_Info;
         Properties: in out External_Semaphore_Properties) is
        C_Semaphore_Info: C_V1_1.Physical_Device_External_Semaphore_Info_C :=
            C_V1_1.To_C(Semaphore_Info);
        C_Properties: C_V1_1.External_Semaphore_Properties_C;
    begin
        C_Properties.Next := Extension_Records.To_C(Properties.Next);
        C_V1_1.vkGetPhysicalDeviceExternalSemaphoreProperties(Device,
                                                              C_Semaphore_Info,
                                                              C_Properties);
        C_V1_1.Free(C_Semaphore_Info);
        C_V1_1.To_Ada(Properties, C_Properties);
        Extension_Records.Free(C_Properties.Next);
    end Get_External_Semaphore_Properties;

    function Get_External_Semaphore_Properties
        (Device: in Physical_Device;
         Semaphore_Info: in Physical_Device_External_Semaphore_Info)
        return External_Semaphore_Properties is
        Properties: External_Semaphore_Properties;
    begin
        Get_External_Semaphore_Properties(Device, Semaphore_Info, Properties);

        return Properties;
    end Get_External_Semaphore_Properties;

    function Tool_Properties_Count(Device: in Physical_Device)
        return Interfaces.Unsigned_32 is
        Count: Interfaces.Unsigned_32 := 0;
    begin
        Exceptions.Check(C_V1_3.vkGetPhysicalDeviceToolProperties(Device,
                                                                  Count,
                                                                  null));

        return Count;
    end Tool_Properties_Count;

    function Get_Tool_Properties
        (Device: in Physical_Device;
         Properties: in out Physical_Device_Tool_Properties_Vectors.Vector)
        return Result is
        use type Interfaces.Unsigned_32;

        Count: Interfaces.Unsigned_32 := Tool_Properties_Count(Device);
        Result: Vulkan.Result;
    begin
        if Count = 0 then
            return Success;
        end if;

        declare
            C_Properties: array (1 .. Count) of
                aliased C_V1_3.Physical_Device_Tool_Properties_C
                with Convention => C;
            A_Properties: Physical_Device_Tool_Properties;
            Index: Interfaces.Unsigned_32 := 1;
        begin
            for P of Properties loop
                C_Properties(Index).Next := Extension_Records.To_C(P.Next);
                Index := Index + 1;
            end loop;

            Result := C_V1_3.vkGetPhysicalDeviceToolProperties
                (Device, Count, C_Properties(1)'Unchecked_Access);
            
            Index := 1;

            for P of Properties loop
                C_V1_3.To_Ada(P, C_Properties(Index));
                Extension_Records.Free(C_Properties(Index).Next);
                Index := Index + 1;
            end loop;
        end;

        return Result;
    end Get_Tool_Properties;

    procedure Get_Tool_Properties
        (Device: in Physical_Device;
         Properties: in out Physical_Device_Tool_Properties_Vectors.Vector) is
    begin
        Exceptions.Check(Get_Tool_Properties(Device, Properties));
    end Get_Tool_Properties;

    function Get_Tool_Properties(Device: in Physical_Device)
        return Physical_Device_Tool_Properties_Vectors.Vector is
        Count: Interfaces.Unsigned_32 := Tool_Properties_Count(Device);
        Item: Physical_Device_Tool_Properties;
        Properties: Physical_Device_Tool_Properties_Vectors.Vector;
    begin
        Properties.Append(Item, Ada.Containers.Count_Type(Count));
        Get_Tool_Properties(Device, Properties);

        return Properties;
    end Get_Tool_Properties;

    function Enumerate_Extension_Properties
        (Device: in Physical_Device;
         Layer_Name: in Interfaces.C.Strings.char_array_access)
        return Extension_Properties_Vectors.Vector is
        use type Interfaces.Unsigned_32;

        Count: Interfaces.Unsigned_32 := 9;
    begin
        if C.vkEnumerateDeviceExtensionProperties(Device,
                                                  Layer_Name,
                                                  Count,
                                                  null) /= Success then
            return Extension_Properties_Vectors.Empty_Vector;
        end if;

        if Count = 0 then
            return Extension_Properties_Vectors.Empty_Vector;
        end if;

        declare
            Properties: Utilities.C_Extension_Properties_Array
                            (1 .. Positive(Count));
        begin
            if C.vkEnumerateDeviceExtensionProperties
                (Device,
                 Layer_Name,
                 Count,
                 Properties(1)'Access) /= Success then
                return Extension_Properties_Vectors.Empty_Vector;
            end if;

            return Utilities.To_Ada(Properties);
        end;
    end Enumerate_Extension_Properties;
end Vulkan.Physical_Devices;

