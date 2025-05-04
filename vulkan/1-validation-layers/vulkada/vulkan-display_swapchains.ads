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

-- Operations for the display swapchains extension

package Vulkan.Display_Swapchains is
    use type Ada.Containers.Count_Type;

    -- Load the extension functions.
    procedure Load_Extension(Instance: in Vulkan.Instance)
        with Pre => Instance /= No_Instance;

    -- vkCreateSharedSwapchainsKHR
    function Create(Device: in Vulkan.Device;
                    Create_Infos: in Swapchain_Create_Info_Vectors.Vector;
                    Allocator: aliased in Allocation_Callbacks;
                    Swapchains: out Swapchain_Vectors.Vector) return Result
        with Pre => Device /= No_Device and not Create_Infos.Is_Empty,
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory |
                                      Incompatible_Display |
                                      Device_Lost |
                                      Surface_Lost and
                     (if Create'Result = Success then
                        (for all Swapchain of Swapchains =>
                            Swapchain /= No_Swapchain));

    function Create(Device: in Vulkan.Device;
                    Create_Infos: in Swapchain_Create_Info_Vectors.Vector;
                    Allocator: aliased in Allocation_Callbacks)
        return Swapchain_Vectors.Vector
        with Pre => Device /= No_Device and not Create_Infos.Is_Empty,
             Post => Create'Result.Length = Create_Infos.Length and
                     (for all Swapchain of Create'Result =>
                        Swapchain /= No_Swapchain);

    function Create(Device: in Vulkan.Device;
                    Create_Infos: in Swapchain_Create_Info_Vectors.Vector;
                    Swapchains: out Swapchain_Vectors.Vector) return Result
        with Pre => Device /= No_Device and
                    not Create_Infos.Is_Empty,
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory |
                                      Incompatible_Display |
                                      Device_Lost |
                                      Surface_Lost and
                     (if Create'Result = Success then
                        (for all Swapchain of Swapchains =>
                            Swapchain /= No_Swapchain));

    function Create(Device: in Vulkan.Device;
                    Create_Infos: in Swapchain_Create_Info_Vectors.Vector)
        return Swapchain_Vectors.Vector            
        with Pre => Device /= No_Device and not Create_Infos.Is_Empty,
             Post => Create'Result.Length = Create_Infos.Length and
                     (for all Swapchain of Create'Result =>
                        Swapchain /= No_Swapchain);

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Swapchain_Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Swapchain: out Vulkan.Swapchain) return Result
        with Pre => Device /= No_Device,
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory |
                                      Incompatible_Display |
                                      Device_Lost |
                                      Surface_Lost;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Swapchain_Create_Info;
                    Allocator: aliased in Allocation_Callbacks) return Swapchain
        with Pre => Device /= No_Device,
             Post => Create'Result /= No_Swapchain;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Swapchain_Create_Info;
                    Swapchain: out Vulkan.Swapchain) return Result
        with Pre => Device /= No_Device,
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory |
                                      Out_Of_Device_Memory |
                                      Incompatible_Display |
                                      Device_Lost |
                                      Surface_Lost;

    function Create(Device: in Vulkan.Device;
                    Create_Info: in Swapchain_Create_Info) return Swapchain           
        with Pre => Device /= No_Device,
             Post => Create'Result /= No_Swapchain;
end Vulkan.Display_Swapchains;

