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

-- Operations for the debug reports extension

package Vulkan.Debug_Reports is
    -- Load the extension functions.
    procedure Load_Extension(Instance: in Vulkan.Instance)
        with Pre => Instance /= No_Instance;

    -- vkCreateDebugReportCallbackEXT
    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in Debug_Report_Callback_Create_Info;
                    Allocator: aliased in Allocation_Callbacks;
                    Callback: out Debug_Report_Callback) return Result
        with Pre => Instance /= No_Instance and
                    Create_Info.Callback /= null,
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory and
                     (if Create'Result = Success then
                        Callback /= No_Debug_Report_Callback);

    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in Debug_Report_Callback_Create_Info;
                    Allocator: aliased in Allocation_Callbacks)
        return Debug_Report_Callback
        with Pre => Instance /= No_Instance and
                    Create_Info.Callback /= null,
             Post => Create'Result /= No_Debug_Report_Callback;

    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in Debug_Report_Callback_Create_Info;
                    Callback: out Debug_Report_Callback) return Result
        with Pre => Instance /= No_Instance and
                    Create_Info.Callback /= null,
             Post => Create'Result in Success |
                                      Out_Of_Host_Memory and
                     (if Create'Result = Success then
                        Callback /= No_Debug_Report_Callback);

    function Create(Instance: in Vulkan.Instance;
                    Create_Info: in Debug_Report_Callback_Create_Info)
        return Debug_Report_Callback
        with Pre => Instance /= No_Instance and
                    Create_Info.Callback /= null,
             Post => Create'Result /= No_Debug_Report_Callback;

    -- vkDestroyDebugReportCallbackEXT
    procedure Destroy(Instance: in Vulkan.Instance;
                      Callback: in out Debug_Report_Callback;
                      Allocator: aliased in Allocation_Callbacks)
        with Pre => Instance /= No_Instance,
             Post => Callback = No_Debug_Report_Callback;

    procedure Destroy(Instance: in Vulkan.Instance;
                      Callback: in out Debug_Report_Callback)
        with Pre => Instance /= No_Instance,
             Post => Callback = No_Debug_Report_Callback;

    -- vkDebugReportMessageEXT
    procedure Message(Instance: in Vulkan.Instance;
                      Flags: in Debug_Report_Flags;
                      Object_Type: in Debug_Report_Object_Type := Debug_Unknown;
                      Object: in Object_Handle := No_Object_Handle;
                      Location: in Interfaces.C.size_t := 0;
                      Message_Code: in Interfaces.Integer_32 := 0;
                      Layer_Prefix, Message: in String := "")
        with Pre => Flags /= Debug_Report_No_Bit;
end Vulkan.Debug_Reports;

