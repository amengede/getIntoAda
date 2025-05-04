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

-- Surface related subprograms

private with Vulkan.C;
private with Vulkan.Utilities;

package Vulkan.Surfaces is
    -- vkDestroySurfaceKHR
    procedure Destroy(Instance: in Vulkan.Instance;
                      Surface: in out Vulkan.Surface;
                      Allocator: aliased in Allocation_Callbacks)
        with Pre => Instance /= No_Instance,
             Post => Surface = No_Surface;

    procedure Destroy(Instance: in Vulkan.Instance;
                      Surface: in out Vulkan.Surface)
        with Pre => Instance /= No_Instance,
             Post => Surface = No_Surface;

    -- vkGetPhysicalDeviceSurfaceSupportKHR
    function Get_Support
        (Device: in Physical_Device;
         Queue_Family_Index: in Vulkan.Queue_Family_Index;
         Surface: in Vulkan.Surface) return Boolean
        with Pre => Device /= No_Physical_Device and
                    Surface /= No_Surface;

    -- vkGetPhysicalDeviceSurfaceCapabilitiesKHR
    function Get_Capabilities
        (Device: in Physical_Device;
         Surface: in Vulkan.Surface) return Surface_Capabilities
        with Pre => Device /= No_Physical_Device and
                    Surface /= No_Surface;

    -- vkGetPhysicalDeviceSurfaceFormatsKHR
    function Get_Formats
        (Device: in Physical_Device;
         Surface: in Vulkan.Surface) return Surface_Format_Vectors.Vector
        with Pre => Device /= No_Physical_Device and
                    Surface /= No_Surface;

    -- vkGetPhysicalDeviceSurfacePresentModesKHR
    function Get_Present_Modes
        (Device: in Physical_Device;
         Surface: in Vulkan.Surface) return Present_Mode_Vectors.Vector
        with Pre => Device /= No_Physical_Device and
                    Surface /= No_Surface;

    -- vkGetDeviceGroupSurfacePresentModesKHR
    function Get_Present_Modes
        (Device: in Vulkan.Device;
         Surface: in Vulkan.Surface) return Device_Group_Present_Mode_Flags
        with Pre => Device /= No_Device and
                    Surface /= No_Surface;

    -- vkGetPhysicalDevicePresentRectanglesKHR
    function Get_Present_Rectangles
        (Device: in Physical_Device;
         Surface: in Vulkan.Surface) return Rect_2D_Vectors.Vector
        with Pre => Device /= No_Physical_Device and
                    Surface /= No_Surface;
         
private
    function Get_Present_Mode_Array is
        new Utilities.Get_Array_2(Physical_Device,
                                  Surface,
                                  Present_Mode_Vectors,
                                  C.vkGetPhysicalDeviceSurfacePresentModesKHR);
   
    function Get_Surface_Format_Array is
        new Utilities.Get_Array_2(Physical_Device,
                                  Surface,
                                  Surface_Format_Vectors,
                                  C.vkGetPhysicalDeviceSurfaceFormatsKHR);

    function Get_Rect_2D_Array is
        new Utilities.Get_Array_2(Physical_Device,
                                  Surface,
                                  Rect_2D_Vectors,
                                  C.vkGetPhysicalDevicePresentRectanglesKHR);

    function Get_Present_Modes
        (Device: in Physical_Device;
         Surface: in Vulkan.Surface) return Present_Mode_Vectors.Vector
        renames Get_Present_Mode_Array;

    function Get_Formats
        (Device: in Physical_Device;
         Surface: in Vulkan.Surface) return Surface_Format_Vectors.Vector
        renames Get_Surface_Format_Array;

    function Get_Present_Rectangles
        (Device: in Physical_Device;
         Surface: in Vulkan.Surface) return Rect_2D_Vectors.Vector
        renames Get_Rect_2D_Array;
end Vulkan.Surfaces;

