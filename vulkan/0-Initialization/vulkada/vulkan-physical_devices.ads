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

-- Physical device routines

private with Vulkan.C;
private with Vulkan.Utilities;

package Vulkan.Physical_Devices is
    -- vkEnumeratePhysicalDevices
    function Enumerate(Instance: in Vulkan.Instance)
        return Physical_Device_Vectors.Vector
        with Pre => Instance /= No_Instance;

    -- vkGetPhysicalDeviceFeatures
    function Get_Features(Device: in Physical_Device)
        return Physical_Device_Features
        with Pre => Device /= No_Physical_Device;

    -- vkGetPhysicalDeviceFormatProperties
    function Get_Format_Properties(Device: in Physical_Device;
                                   Format: in Vulkan.Format)
        return Format_Properties
        with Inline,
             Pre => Device /= No_Physical_Device;

    -- vkGetPhysicalDeviceImageFormatProperties
    function Get_Image_Format_Properties
        (Device: in Physical_Device;
         Format: in Vulkan.Format;
         Image_Type: in Vulkan.Image_Type;
         Tiling: in Image_Tiling;
         Usage: in Image_Usage_Flags;
         Create_Flags: in Image_Create_Flags;
         Properties: out Image_Format_Properties) return Result
        with Inline,
             Pre => Device /= No_Physical_Device and
                    Tiling /= DRM_Format_Modifier and
                    Usage /= Image_Usage_No_Bit,
             Post => Get_Image_Format_Properties'Result in 
                        Success |
                        Out_Of_Host_Memory |
                        Out_Of_Device_Memory |
                        Format_Not_Supported;

    function Get_Image_Format_Properties(Device: in Physical_Device;
                                         Format: in Vulkan.Format;
                                         Image_Type: in Vulkan.Image_Type;
                                         Tiling: in Image_Tiling;
                                         Usage: in Image_Usage_Flags;
                                         Create_Flags: in Image_Create_Flags)
        return Image_Format_Properties
        with Inline,
             Pre => Device /= No_Physical_Device and
                    Tiling /= DRM_Format_Modifier and
                    Usage /= Image_Usage_No_Bit;

    -- vkGetPhysicalDeviceProperties
    function Get_Properties(Device: in Physical_Device)
        return Physical_Device_Properties
        with Inline,
             Pre => Device /= No_Physical_Device;

    -- vkGetPhysicalDeviceQueueFamilyProperties
    function Get_Queue_Family_Properties(Device: in Physical_Device)
        return Queue_Family_Properties_Vectors.Vector
        with Pre => Device /= No_Physical_Device;

    -- vkGetPhysicalDeviceMemoryProperties
    function Get_Memory_Properties(Device: in Physical_Device)
        return Physical_Device_Memory_Properties
        with Pre => Device /= No_Physical_Device;

    -- vkEnumerateDeviceExtensionProperties
    function Enumerate_Extension_Properties(Device: in Physical_Device;
                                            Layer_Name: in String)
        return Extension_Properties_Vectors.Vector
        with Pre => Device /= No_Physical_Device;

    function Enumerate_Extension_Properties(Device: in Physical_Device)
        return Extension_Properties_Vectors.Vector
        with Pre => Device /= No_Physical_Device;

    -- vkEnumerateDeviceLayerProperties
    function Enumerate_Layer_Properties(Device: in Physical_Device)
        return Layer_Properties_Vectors.Vector
        with Pre => Device /= No_Physical_Device;

    -- vkGetPhysicalDeviceSparseImageFormatProperties
    function Get_Sparse_Image_Format_Properties
        (Device: in Physical_Device;
         Format: in Vulkan.Format;
         Image_Type: in Vulkan.Image_Type;
         Samples: in Sample_Count_Flags;
         Usage: in Image_Usage_Flags;
         Tiling: in Image_Tiling)
        return Sparse_Image_Format_Properties_Vectors.Vector
        with Pre => Device /= No_Physical_Device and
                    Usage /= Image_Usage_No_Bit;

    -- Vulkan 1.1
    -- vkEnumeratePhysicalDeviceGroups
    function Group_Properties_Count(Instance: in Vulkan.Instance)
        return Interfaces.Unsigned_32
        with Pre => Instance /= No_Instance;

    function Enumerate
        (Instance: in Vulkan.Instance;
         Properties: in out Physical_Device_Group_Properties_Vectors.Vector)
        return Result
        with Pre => Instance /= No_Instance,
             Post => Enumerate'Result in Success |
                                         Incomplete |
                                         Out_Of_Host_Memory |
                                         Out_Of_Device_Memory |
                                         Initialization_Failed;

    function Enumerate(Instance: in Vulkan.Instance)
        return Physical_Device_Group_Properties_Vectors.Vector
        with Pre => Instance /= No_Instance;

    -- vkGetPhysicalDeviceFeatures2
    procedure Get_Features(Device: in Physical_Device;
                           Features: in out Physical_Device_Features_2)
        with Pre => Device /= No_Physical_Device;

    function Get_Features(Device: in Physical_Device)
        return Physical_Device_Features_2
        with Pre => Device /= No_Physical_Device;

    -- vkGetPhysicalDeviceProperties2
    procedure Get_Properties(Device: in Physical_Device;
                             Properties: in out Physical_Device_Properties_2)
        with Pre => Device /= No_Physical_Device;

    function Get_Properties(Device: in Physical_Device)
        return Physical_Device_Properties_2
        with Pre => Device /= No_Physical_Device;

    -- vkGetPhysicalDeviceFormatProperties2
    procedure Get_Format_Properties(Device: in Physical_Device;
                                    Format: in Vulkan.Format;
                                    Properties: in out Format_Properties_2)
        with Pre => Device /= No_Physical_Device;

    function Get_Format_Properties(Device: in Physical_Device;
                                   Format: in Vulkan.Format)
        return Format_Properties_2
        with Pre => Device /= No_Physical_Device;

    -- vkGetPhysicalDeviceImageFormatProperties2
    function Get_Image_Format_Properties
        (Device: in Physical_Device;
         Image_Format_Info: in Physical_Device_Image_Format_Info_2;
         Image_Format_Properties: in out Image_Format_Properties_2)
        return Result
        with Pre => Device /= No_Physical_Device and
                    Image_Format_Info.Usage /= Image_Usage_No_Bit,
             Post => Get_Image_Format_Properties'Result in
                        Success |
                        Out_Of_Host_Memory |
                        Out_Of_Device_Memory |
                        Format_Not_Supported;

    function Get_Image_Format_Properties
        (Device: in Physical_Device;
         Image_Format_Info: in Physical_Device_Image_Format_Info_2)
        return Image_Format_Properties_2
        with Pre => Device /= No_Physical_Device and
                    Image_Format_Info.Usage /= Image_Usage_No_Bit;

    -- vkGetPhysicalDeviceQueueFamilyProperties2
    function Queue_Family_Properties_Count(Device: in Physical_Device)
        return Interfaces.Unsigned_32
        with Pre => Device /= No_Physical_Device;

    procedure Get_Queue_Family_Properties
        (Device: in Physical_Device;
         Properties: in out Queue_Family_Properties_2_Vectors.Vector)
        with Pre => Device /= No_Physical_Device;

    function Get_Queue_Family_Properties(Device: in Physical_Device)
        return Queue_Family_Properties_2_Vectors.Vector
        with Pre => Device /= No_Physical_Device;

    -- vkGetPhysicalDeviceMemoryProperties2
    procedure Get_Memory_Properties
        (Device: in Physical_Device;
         Properties: in out Physical_Device_Memory_Properties_2)
        with Pre => Device /= No_Physical_Device;

    function Get_Memory_Properties(Device: in Physical_Device)
        return Physical_Device_Memory_Properties_2
        with Pre => Device /= No_Physical_Device;

    -- vkGetPhysicalDeviceSparseImageFormatProperties2
    function Sparse_Image_Format_Properties_Count
        (Device: in Physical_Device;
         Format_Info: in Physical_Device_Sparse_Image_Format_Info_2)
        return Interfaces.Unsigned_32
        with Pre => Device /= No_Physical_Device and
                    Format_Info.Usage /= Image_Usage_No_Bit;

    procedure Get_Sparse_Image_Format_Properties
        (Device: in Physical_Device;
         Format_Info: in Physical_Device_Sparse_Image_Format_Info_2;
         Properties: in out Sparse_Image_Format_Properties_2_Vectors.Vector)
        with Pre => Device /= No_Physical_Device and
                    Format_Info.Usage /= Image_Usage_No_Bit;

    function Get_Sparse_Image_Format_Properties
        (Device: in Physical_Device;
         Format_Info: in Physical_Device_Sparse_Image_Format_Info_2)
        return Sparse_Image_Format_Properties_2_Vectors.Vector
        with Pre => Device /= No_Physical_Device and
                    Format_Info.Usage /= Image_Usage_No_Bit;

    -- vkGetPhysicalDeviceExternalBufferProperties
    procedure Get_External_Buffer_Properties
        (Device: in Physical_Device;
         Buffer_Info: in Physical_Device_External_Buffer_Info;
         Properties: in out External_Buffer_Properties)
        with Pre => Device /= No_Physical_Device and
                    Buffer_Info.Usage /= Buffer_Usage_No_Bit;
                                            
    function Get_External_Buffer_Properties
        (Device: in Physical_Device;
         Buffer_Info: in Physical_Device_External_Buffer_Info)
        return External_Buffer_Properties
        with Pre => Device /= No_Physical_Device and
                    Buffer_Info.Usage /= Buffer_Usage_No_Bit;

    -- vkGetPhysicalDeviceExternalFenceProperties
    procedure Get_External_Fence_Properties
        (Device: in Physical_Device;
         Fence_Info: in Physical_Device_External_Fence_Info;
         Properties: in out External_Fence_Properties)
        with Pre => Device /= No_Physical_Device;

    function Get_External_Fence_Properties
        (Device: in Physical_Device;
         Fence_Info: in Physical_Device_External_Fence_Info)
        return External_Fence_Properties
        with Pre => Device /= No_Physical_Device;

    -- vkGetPhysicalDeviceExternalSemaphoreProperties
    procedure Get_External_Semaphore_Properties
        (Device: in Physical_Device;
         Semaphore_Info: in Physical_Device_External_Semaphore_Info;
         Properties: in out External_Semaphore_Properties)
        with Pre => Device /= No_Physical_Device;

    function Get_External_Semaphore_Properties
        (Device: in Physical_Device;
         Semaphore_Info: in Physical_Device_External_Semaphore_Info)
        return External_Semaphore_Properties
        with Pre => Device /= No_Physical_Device;

    -- Vulkan 1.3
    -- vkGetPhysicalDeviceToolProperties
    function Tool_Properties_Count(Device: in Physical_Device)
        return Interfaces.Unsigned_32
        with Pre => Device /= No_Physical_Device;

    function Get_Tool_Properties
        (Device: in Physical_Device;
         Properties: in out Physical_Device_Tool_Properties_Vectors.Vector)
        return Result
        with Pre => Device /= No_Physical_Device,
             Post => Get_Tool_Properties'Result in Success |
                                                   Incomplete |
                                                   Out_Of_Host_Memory;

    procedure Get_Tool_Properties
        (Device: in Physical_Device;
         Properties: in out Physical_Device_Tool_Properties_Vectors.Vector)
        with Inline,
             Pre => Device /= No_Physical_Device;

    function Get_Tool_Properties(Device: in Physical_Device)
        return Physical_Device_Tool_Properties_Vectors.Vector
        with Pre => Device /= No_Physical_Device;

private
    function Get_Physical_Device_Array is
        new Utilities.Get_Array_1(Instance,
                                  Physical_Device_Vectors,
                                  C.vkEnumeratePhysicalDevices);

    function Get_Queue_Family_Properties_Array is
        new Utilities.Get_Array_1_Proc(Physical_Device,
                                       Queue_Family_Properties_Vectors,
                                       C.vkGetPhysicalDeviceQueueFamilyProperties);

    function Enumerate(Instance: in Vulkan.Instance)
        return Physical_Device_Vectors.Vector
        renames Get_Physical_Device_Array;
    
    function Get_Queue_Family_Properties(Device: in Physical_Device)
        return Queue_Family_Properties_Vectors.Vector
        renames Get_Queue_Family_Properties_Array;
end Vulkan.Physical_Devices;

