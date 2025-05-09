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

-- Subprogram access for Vulkan 1.1

with Ada.Unchecked_Conversion;
with Vulkan.Core;
with Vulkan.Utilities;
with Vulkan.Extension_Records;

package body Vulkan.C_V1_1 is
    procedure Load(Instance: in Vulkan.Instance) is
        generic
            type Pointer(<>) is private;
        procedure Load_Pointer(P: out Pointer; Name: in String);

        procedure Load_Pointer(P: out Pointer; Name: in String) is
            function Get_Pointer is new Core.Get_Proc_Addr(Pointer);
        begin
            P := Get_Pointer(Instance, Name);
        end Load_Pointer;

        procedure Load is
            new Load_Pointer(vkBindBufferMemory2_Access);
        procedure Load is
            new Load_Pointer(vkBindImageMemory2_Access);
        procedure Load is
            new Load_Pointer(vkGetDeviceGroupPeerMemoryFeatures_Access);
        procedure Load is
            new Load_Pointer(vkCmdSetDeviceMask_Access);
        procedure Load is
            new Load_Pointer(vkCmdDispatchBase_Access);
        procedure Load is
            new Load_Pointer(vkEnumeratePhysicalDeviceGroups_Access);
        procedure Load is
            new Load_Pointer(vkGetImageMemoryRequirements2_Access);
        procedure Load is
            new Load_Pointer(vkGetBufferMemoryRequirements2_Access);
        procedure Load is
            new Load_Pointer(vkGetImageSparseMemoryRequirements2_Access);
        procedure Load is
            new Load_Pointer(vkGetPhysicalDeviceFeatures2_Access);
        procedure Load is
            new Load_Pointer(vkGetPhysicalDeviceProperties2_Access);
        procedure Load is
            new Load_Pointer(vkGetPhysicalDeviceFormatProperties2_Access);
        procedure Load is
            new Load_Pointer(vkGetPhysicalDeviceImageFormatProperties2_Access);
        procedure Load is
            new Load_Pointer(vkGetPhysicalDeviceQueueFamilyProperties2_Access);
        procedure Load is
            new Load_Pointer(vkGetPhysicalDeviceMemoryProperties2_Access);
        procedure Load is
            new Load_Pointer
                (vkGetPhysicalDeviceSparseImageFormatProperties2_Access);
        procedure Load is new Load_Pointer(vkTrimCommandPool_Access);
        procedure Load is new Load_Pointer(vkGetDeviceQueue2_Access);
        procedure Load is
            new Load_Pointer(vkCreateSamplerYcbcrConversion_Access);
        procedure Load is
            new Load_Pointer(vkDestroySamplerYcbcrConversion_Access);
        procedure Load is
            new Load_Pointer(vkCreateDescriptorUpdateTemplate_Access);
        procedure Load is
            new Load_Pointer(vkDestroyDescriptorUpdateTemplate_Access);
        procedure Load is
            new Load_Pointer(vkUpdateDescriptorSetWithTemplate_Access);
        procedure Load is
            new Load_Pointer
                (vkGetPhysicalDeviceExternalBufferProperties_Access);
        procedure Load is
            new Load_Pointer
                (vkGetPhysicalDeviceExternalFenceProperties_Access);
        procedure Load is
            new Load_Pointer
                (vkGetPhysicalDeviceExternalSemaphoreProperties_Access);
        procedure Load is
            new Load_Pointer(vkGetDescriptorSetLayoutSupport_Access);
    begin
        Load(vkBindBufferMemory2, "vkBindBufferMemory2");
        Load(vkBindImageMemory2, "vkBindImageMemory2");
        Load(vkGetDeviceGroupPeerMemoryFeatures,
             "vkGetDeviceGroupPeerMemoryFeatures");
        Load(vkCmdSetDeviceMask, "vkCmdSetDeviceMask");
        Load(vkCmdDispatchBase, "vkCmdDispatchBase");
        Load(vkEnumeratePhysicalDeviceGroups,
             "vkEnumeratePhysicalDeviceGroups");
        Load(vkGetImageMemoryRequirements2, "vkGetImageMemoryRequirements2");
        Load(vkGetBufferMemoryRequirements2, "vkGetBufferMemoryRequirements2");
        Load(vkGetImageSparseMemoryRequirements2,
             "vkGetImageSparseMemoryRequirements2");
        Load(vkGetPhysicalDeviceFeatures2, "vkGetPhysicalDeviceFeatures2");
        Load(vkGetPhysicalDeviceProperties2, "vkGetPhysicalDeviceProperties2");
        Load(vkGetPhysicalDeviceFormatProperties2,
             "vkGetPhysicalDeviceFormatProperties2");
        Load(vkGetPhysicalDeviceImageFormatProperties2,
             "vkGetPhysicalDeviceImageFormatProperties2");
        Load(vkGetPhysicalDeviceQueueFamilyProperties2,
             "vkGetPhysicalDeviceQueueFamilyProperties2");
        Load(vkGetPhysicalDeviceMemoryProperties2,
             "vkGetPhysicalDeviceMemoryProperties2");
        Load(vkGetPhysicalDeviceSparseImageFormatProperties2,
             "vkGetPhysicalDeviceSparseImageFormatProperties2");
        Load(vkTrimCommandPool, "vkTrimCommandPool");
        Load(vkGetDeviceQueue2, "vkGetDeviceQueue2");
        Load(vkCreateSamplerYcbcrConversion, "vkCreateSamplerYcbcrConversion");
        Load(vkDestroySamplerYcbcrConversion,
             "vkDestroySamplerYcbcrConversion");
        Load(vkCreateDescriptorUpdateTemplate,
             "vkCreateDescriptorUpdateTemplate");
        Load(vkDestroyDescriptorUpdateTemplate,
             "vkDestroyDescriptorUpdateTemplate");
        Load(vkUpdateDescriptorSetWithTemplate,
             "vkUpdateDescriptorSetWithTemplate");
        Load(vkGetPhysicalDeviceExternalBufferProperties,
             "vkGetPhysicalDeviceExternalBufferProperties");
        Load(vkGetPhysicalDeviceExternalFenceProperties,
             "vkGetPhysicalDeviceExternalFenceProperties");
        Load(vkGetPhysicalDeviceExternalSemaphoreProperties,
             "vkGetPhysicalDeviceExternalSemaphoreProperties");
        Load(vkGetDescriptorSetLayoutSupport,
             "vkGetDescriptorSetLayoutSupport");
    end Load;

    procedure To_Ada(Ada_Struct: in out Physical_Device_Subgroup_Properties;
                     C_Struct: in Physical_Device_Subgroup_Properties_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Subgroup_Size := C_Struct.Subgroup_Size;
        Ada_Struct.Supported_Stages := C_Struct.Supported_Stages;
        Ada_Struct.Supported_Operations := C_Struct.Supported_Operations;
        Ada_Struct.Quad_Operations_In_All_Stages :=
            Utilities.To_Ada(C_Struct.Quad_Operations_In_All_Stages);
    end To_Ada;

    function To_C(Struct: in Bind_Buffer_Memory_Info)
        return Bind_Buffer_Memory_Info_C is
        BBMIC: Bind_Buffer_Memory_Info_C;
    begin
        BBMIC.Next := Extension_Records.To_C(Struct.Next);
        BBMIC.Buffer := Struct.Buffer;
        BBMIC.Memory := Struct.Memory;
        BBMIC.Memory_Offset := Struct.Memory_Offset;

        return BBMIC;
    end To_C;

    procedure Free(Struct: in out Bind_Buffer_Memory_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Bind_Image_Memory_Info)
        return Bind_Image_Memory_Info_C is
        BIMIC: Bind_Image_Memory_Info_C;
    begin
        BIMIC.Next := Extension_Records.To_C(Struct.Next);
        BIMIC.Image := Struct.Image;
        BIMIC.Memory := Struct.Memory;
        BIMIC.Memory_Offset := Struct.Memory_Offset;

        return BIMIC;
    end To_C;

    procedure Free(Struct: in out Bind_Image_Memory_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    procedure To_Ada(Ada_Struct: in out Physical_Device_16Bit_Storage_Features;
                     C_Struct: in Physical_Device_16Bit_Storage_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Storage_Buffer_16Bit_Access :=
            Utilities.To_Ada(C_Struct.Storage_Buffer_16Bit_Access);
        Ada_Struct.Uniform_And_Storage_Buffer_16Bit_Access :=
            Utilities.To_Ada(C_Struct.Uniform_And_Storage_Buffer_16Bit_Access);
        Ada_Struct.Storage_Push_Constant_16 :=
            Utilities.To_Ada(C_Struct.Storage_Push_Constant_16);
        Ada_Struct.Storage_Input_Output_16 :=
            Utilities.To_Ada(C_Struct.Storage_Input_Output_16);
    end To_Ada;

    procedure To_Ada(Ada_Struct: in out Memory_Dedicated_Requirements;
                     C_Struct: in Memory_Dedicated_Requirements_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Prefers_Dedicated_Allocation :=
            Utilities.To_Ada(C_Struct.Prefers_Dedicated_Allocation);
        Ada_Struct.Requires_Dedicated_Allocation :=
            Utilities.To_Ada(C_Struct.Requires_Dedicated_Allocation);
    end To_Ada;

    function To_C(Struct: in Memory_Dedicated_Allocate_Info)
        return Memory_Dedicated_Allocate_Info_C is
        MDAIC: Memory_Dedicated_Allocate_Info_C;
    begin
        MDAIC.Next := Extension_Records.To_C(Struct.Next);
        MDAIC.Image := Struct.Image;
        MDAIC.Buffer := Struct.Buffer;

        return MDAIC;
    end To_C;

    procedure Free(Struct: in out Memory_Dedicated_Allocate_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Memory_Allocate_Flags_Info)
        return Memory_Allocate_Flags_Info_C is
        MAFIC: Memory_Allocate_Flags_Info_C;
    begin
        MAFIC.Next := Extension_Records.To_C(Struct.Next);
        MAFIC.Flags := Struct.Flags;
        MAFIC.Device_Mask := Struct.Device_Mask;

        return MAFIC;
    end To_C;

    procedure Free(Struct: in out Memory_Allocate_Flags_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Device_Group_Render_Pass_Begin_Info)
        return Device_Group_Render_Pass_Begin_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array(C.Rect_2D_Arrays,
                                                         Rect_2D_Vectors);
                                
        DGRPBIC: Device_Group_Render_Pass_Begin_Info_C;
    begin
        DGRPBIC.Next := Extension_Records.To_C(Struct.Next);
        DGRPBIC.Device_Mask := Struct.Device_Mask;
        To_C_Array(DGRPBIC.Device_Render_Area_Count,
                   Struct.Device_Render_Areas,
                   DGRPBIC.Device_Render_Areas);

        return DGRPBIC;
    end To_C;

    procedure Free(Struct: in out Device_Group_Render_Pass_Begin_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        C.Rect_2D_Arrays.Free(Struct.Device_Render_Areas);
    end Free;

    function To_C(Struct: in Device_Group_Command_Buffer_Begin_Info)
        return Device_Group_Command_Buffer_Begin_Info_C is
        DGCBBIC: Device_Group_Command_Buffer_Begin_Info_C;
    begin
        DGCBBIC.Next := Extension_Records.To_C(Struct.Next);
        DGCBBIC.Device_Mask := Struct.Device_Mask;

        return DGCBBIC;
    end To_C;

    procedure Free(Struct: in out Device_Group_Command_Buffer_Begin_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Device_Group_Submit_Info)
        return Device_Group_Submit_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array(C.Uint32_t_Arrays,
                                                         Unsigned_32_Vectors);

        DGSIC: Device_Group_Submit_Info_C;
    begin
        DGSIC.Next := Extension_Records.To_C(Struct.Next);
        To_C_Array(DGSIC.Wait_Semaphore_Count,
                   Struct.Wait_Semaphore_Device_Indices,
                   DGSIC.Wait_Semaphore_Device_Indices);
        To_C_Array(DGSIC.Command_Buffer_Count,
                   Struct.Command_Buffer_Device_Masks,
                   DGSIC.Command_Buffer_Device_Masks);
        To_C_Array(DGSIC.Signal_Semaphore_Count,
                   Struct.Signal_Semaphore_Device_Indices,
                   DGSIC.Signal_Semaphore_Device_Indices);

        return DGSIC;
    end To_C;

    procedure Free(Struct: in out Device_Group_Submit_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        C.Uint32_t_Arrays.Free(Struct.Wait_Semaphore_Device_Indices);
        C.Uint32_t_Arrays.Free(Struct.Command_Buffer_Device_Masks);
        C.Uint32_t_Arrays.Free(Struct.Signal_Semaphore_Device_Indices);
    end Free;

    function To_C(Struct: in Device_Group_Bind_Sparse_Info)
        return Device_Group_Bind_Sparse_Info_C is
        DGBSIC: Device_Group_Bind_Sparse_Info_C;
    begin
        DGBSIC.Next := Extension_Records.To_C(Struct.Next);
        DGBSIC.Resource_Device_Index := Struct.Resource_Device_Index;
        DGBSIC.Memory_Device_Index := Struct.Memory_Device_Index;

        return DGBSIC;
    end To_C;

    procedure Free(Struct: in out Device_Group_Bind_Sparse_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Bind_Buffer_Memory_Device_Group_Info)
        return Bind_Buffer_Memory_Device_Group_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array(C.Uint32_t_Arrays,
                                                         Unsigned_32_Vectors);

        BBMDGIC: Bind_Buffer_Memory_Device_Group_Info_C;
    begin
        BBMDGIC.Next := Extension_Records.To_C(Struct.Next);
        To_C_Array(BBMDGIC.Device_Index_Count,
                   Struct.Device_Indices,
                   BBMDGIC.Device_Indices);

        return BBMDGIC;
    end To_C;

    procedure Free(Struct: in out Bind_Buffer_Memory_Device_Group_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        C.Uint32_t_Arrays.Free(Struct.Device_Indices);
    end Free;

    function To_C(Struct: in Bind_Image_Memory_Device_Group_Info)
        return Bind_Image_Memory_Device_Group_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array(C.Uint32_t_Arrays,
                                                         Unsigned_32_Vectors);
        procedure To_C_Array is new Utilities.To_C_Array(C.Rect_2D_Arrays,
                                                         Rect_2D_Vectors);

        BIMDGIC: Bind_Image_Memory_Device_Group_Info_C;
    begin
        BIMDGIC.Next := Extension_Records.To_C(Struct.Next);
        To_C_Array(BIMDGIC.Device_Index_Count,
                   Struct.Device_Indices,
                   BIMDGIC.Device_Indices);
        To_C_Array(BIMDGIC.Split_Instance_Bind_Region_Count,
                   Struct.Split_Instance_Bind_Regions,
                   BIMDGIC.Split_Instance_Bind_Regions);

        return BIMDGIC;
    end To_C;

    procedure Free(Struct: in out Bind_Image_Memory_Device_Group_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        C.Uint32_t_Arrays.Free(Struct.Device_Indices);
        C.Rect_2D_Arrays.Free(Struct.Split_Instance_Bind_Regions);
    end Free;

    procedure To_Ada(Ada_Struct: in out Physical_Device_Group_Properties;
                     C_Struct: in Physical_Device_Group_Properties_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);

        Ada_Struct.Physical_Devices.Clear;

        for X in 1 .. C_Struct.Physical_Device_Count loop
            Ada_Struct.Physical_Devices.Append
                (C_Struct.Physical_Devices(Integer(X)));
        end loop;

        Ada_Struct.Subset_Allocation :=
            Utilities.To_Ada(C_Struct.Subset_Allocation);
    end To_Ada;

    function To_C(Struct: in Device_Group_Device_Create_Info)
        return Device_Group_Device_Create_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array
            (Physical_Device_Arrays, Physical_Device_Vectors);

        DGDCI: Device_Group_Device_Create_Info_C;
    begin
        DGDCI.Next := Extension_Records.To_C(Struct.Next);
        To_C_Array(DGDCI.Physical_Device_Count,
                   Struct.Physical_Devices,
                   DGDCI.Physical_Devices);

        return DGDCI;
    end To_C;

    procedure Free(Struct: in out Device_Group_Device_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        Physical_Device_Arrays.Free(Struct.Physical_Devices);
    end Free;

    function To_C(Struct: in Buffer_Memory_Requirements_Info_2)
        return Buffer_Memory_Requirements_Info_2_C is
        BMRI2C: Buffer_Memory_Requirements_Info_2_C;
    begin
        BMRI2C.Next := Extension_Records.To_C(Struct.Next);
        BMRI2C.Buffer := Struct.Buffer;

        return BMRI2C;
    end To_C;

    procedure Free(Struct: in out Buffer_Memory_Requirements_Info_2_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Image_Memory_Requirements_Info_2)
        return Image_Memory_Requirements_Info_2_C is
        IMRI2C: Image_Memory_Requirements_Info_2_C;
    begin
        IMRI2C.Next := Extension_Records.To_C(Struct.Next);
        IMRI2C.Image := Struct.Image;

        return IMRI2C;
    end To_C;

    procedure Free(Struct: in out Image_Memory_Requirements_Info_2_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Image_Sparse_Memory_Requirements_Info_2)
        return Image_Sparse_Memory_Requirements_Info_2_C is
        ISMRI2C: Image_Sparse_Memory_Requirements_Info_2_C;
    begin
        ISMRI2C.Next := Extension_Records.To_C(Struct.Next);
        ISMRI2C.Image := Struct.Image;

        return ISMRI2C;
    end To_C;

    procedure Free(Struct: in out Image_Sparse_Memory_Requirements_Info_2_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    procedure To_Ada(Ada_Struct: in out Memory_Requirements_2;
                     C_Struct: in Memory_Requirements_2_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Memory_Requirements := C_Struct.Memory_Requirements;
    end To_Ada;

    procedure To_Ada(Ada_Struct: in out Sparse_Image_Memory_Requirements_2;
                     C_Struct: in Sparse_Image_Memory_Requirements_2_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Memory_Requirements := C_Struct.Memory_Requirements;
    end To_Ada;

    procedure To_Ada(Ada_Struct: in out Physical_Device_Features_2;
                     C_Struct: in Physical_Device_Features_2_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Features := Utilities.To_Ada(C_Struct.Features);
    end To_Ada;

    procedure To_Ada(Ada_Struct: in out Physical_Device_Properties_2;
                     C_Struct: in Physical_Device_Properties_2_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Properties := Utilities.To_Ada(C_Struct.Properties);
    end To_Ada;

    procedure To_Ada(Ada_Struct: in out Format_Properties_2;
                     C_Struct: in Format_Properties_2_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Format_Properties := C_Struct.Format_Properties;
    end To_Ada;

    procedure To_Ada(Ada_Struct: in out Image_Format_Properties_2;
                     C_Struct: in Image_Format_Properties_2_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Image_Format_Properties := C_Struct.Image_Format_Properties;
    end To_Ada;

    function To_C(Struct: in Physical_Device_Image_Format_Info_2)
        return Physical_Device_Image_Format_Info_2_C is
        PDIFI2C: Physical_Device_Image_Format_Info_2_C;
    begin
        PDIFI2C.Next := Extension_Records.To_C(Struct.Next);
        PDIFI2C.Format := Struct.Format;
        PDIFI2C.Image_Type := Struct.Image_Type;
        PDIFI2C.Tiling := Struct.Tiling;
        PDIFI2C.Usage := Struct.Usage;
        PDIFI2C.Flags := Struct.Flags;

        return PDIFI2C;
    end To_C;

    procedure Free(Struct: in out Physical_Device_Image_Format_Info_2_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    procedure To_Ada(Ada_Struct: in out Queue_Family_Properties_2;
                     C_Struct: in Queue_Family_Properties_2_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Queue_Family_Properties := C_Struct.Queue_Family_Properties;
    end To_Ada;

    procedure To_Ada(Ada_Struct: in out Physical_Device_Memory_Properties_2;
                     C_Struct: in Physical_Device_Memory_Properties_2_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Memory_Properties :=
            Utilities.To_Ada(C_Struct.Memory_Properties);
    end To_Ada;

    procedure To_Ada(Ada_Struct: in out Sparse_Image_Format_Properties_2;
                     C_Struct: in Sparse_Image_Format_Properties_2_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Properties := C_Struct.Properties;
    end To_Ada;

    function To_C(Struct: in Physical_Device_Sparse_Image_Format_Info_2)
        return Physical_Device_Sparse_Image_Format_Info_2_C is
        PDSIFI2C: Physical_Device_Sparse_Image_Format_Info_2_C;
    begin
        PDSIFI2C.Next := Extension_Records.To_C(Struct.Next);
        PDSIFI2C.Format := Struct.Format;
        PDSIFI2C.Image_Type := Struct.Image_Type;
        PDSIFI2C.Samples := Struct.Samples;
        PDSIFI2C.Usage := Struct.Usage;
        PDSIFI2C.Tiling := Struct.Tiling;

        return PDSIFI2C;
    end To_C;

    procedure Free
        (Struct: in out Physical_Device_Sparse_Image_Format_Info_2_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Point_Clipping_Properties;
         C_Struct: in Physical_Device_Point_Clipping_Properties_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Point_Clipping_Behavior := C_Struct.Point_Clipping_Behavior;
    end To_Ada;

    function To_C(Struct: in Render_Pass_Input_Attachment_Aspect_Create_Info)
        return Render_Pass_Input_Attachment_Aspect_Create_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array
            (Input_Attachment_Aspect_Reference_Arrays,
             Input_Attachment_Aspect_Reference_Vectors);

        RPIAACIC: Render_Pass_Input_Attachment_Aspect_Create_Info_C;
    begin
        RPIAACIC.Next := Extension_Records.To_C(Struct.Next);
        To_C_Array(RPIAACIC.Aspect_Reference_Count,
                   Struct.Aspect_References,
                   RPIAACIC.Aspect_References);

        return RPIAACIC;
    end To_C;

    procedure Free
        (Struct: in out Render_Pass_Input_Attachment_Aspect_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        Input_Attachment_Aspect_Reference_Arrays.Free(Struct.Aspect_References);
    end Free;

    function To_C(Struct: in Image_View_Usage_Create_Info)
        return Image_View_Usage_Create_Info_C is
        IVUCUC: Image_View_Usage_Create_Info_C;
    begin
        IVUCUC.Next := Extension_Records.To_C(Struct.Next);
        IVUCUC.Usage := Struct.Usage;

        return IVUCUC;
    end To_C;

    procedure Free(Struct: in out Image_View_Usage_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C
        (Struct: in Pipeline_Tessellation_Domain_Origin_State_Create_Info)
        return Pipeline_Tessellation_Domain_Origin_State_Create_Info_C is
        PTDOSCIC: Pipeline_Tessellation_Domain_Origin_State_Create_Info_C;
    begin
        PTDOSCIC.Next := Extension_Records.To_C(Struct.Next);
        PTDOSCIC.Domain_Origin := Struct.Domain_Origin;

        return PTDOSCIC;
    end To_C;

    procedure Free
        (Struct: in out
            Pipeline_Tessellation_Domain_Origin_State_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Render_Pass_Multiview_Create_Info)
        return Render_Pass_Multiview_Create_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array(C.Uint32_t_Arrays,
                                                         Unsigned_32_Vectors);

        RPMCIC: Render_Pass_Multiview_Create_Info_C;
    begin
        RPMCIC.Next := Extension_Records.To_C(Struct.Next);
        To_C_Array(RPMCIC.Subpass_Count,
                   Struct.View_Masks,
                   RPMCIC.View_Masks);
        To_C_Array(RPMCIC.Dependency_Count,
                   Struct.View_Offsets,
                   RPMCIC.View_Offsets);
        To_C_Array(RPMCIC.Correlation_Mask_Count,
                   Struct.Correlation_Masks,
                   RPMCIC.Correlation_Masks);

        return RPMCIC;
    end To_C;

    procedure Free(Struct: in out Render_Pass_Multiview_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        C.Uint32_t_Arrays.Free(Struct.View_Masks);
        C.Uint32_t_Arrays.Free(Struct.View_Offsets);
        C.Uint32_t_Arrays.Free(Struct.Correlation_Masks);
    end Free;

    procedure To_Ada(Ada_Struct: in out Physical_Device_Multiview_Features;
                     C_Struct: in Physical_Device_Multiview_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Multiview := Utilities.To_Ada(C_Struct.Multiview);
        Ada_Struct.Multiview_Geometry_Shader :=
            Utilities.To_Ada(C_Struct.Multiview_Geometry_Shader);
        Ada_Struct.Multiview_Tessellation_Shader :=
            Utilities.To_Ada(C_Struct.Multiview_Tessellation_Shader);
    end To_Ada;

    procedure To_Ada(Ada_Struct: in out Physical_Device_Multiview_Properties;
                     C_Struct: in Physical_Device_Multiview_Properties_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Max_Multiview_View_Count :=
            C_Struct.Max_Multiview_View_Count;
        Ada_Struct.Max_Multiview_Instance_Index :=
            C_Struct.Max_Multiview_Instance_Index;
    end To_Ada;

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Variable_Pointer_Features;
         C_Struct: in Physical_Device_Variable_Pointer_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Variable_Pointers_Storage_Buffer :=
            Utilities.To_Ada(C_Struct.Variable_Pointers_Storage_Buffer);
        Ada_Struct.Variable_Pointers :=
            Utilities.To_Ada(C_Struct.Variable_Pointers);
    end To_Ada;

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Protected_Memory_Features;
         C_Struct: in Physical_Device_Protected_Memory_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Protected_Memory :=
            Utilities.To_Ada(C_Struct.Protected_Memory);
    end To_Ada;

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Protected_Memory_Properties;
         C_Struct: in Physical_Device_Protected_Memory_Properties_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Protected_No_Fault :=
            Utilities.To_Ada(C_Struct.Protected_No_Fault);
    end To_Ada;

    function To_C(Struct: in Device_Queue_Info_2)
        return Device_Queue_Info_2_C is
        DQI2C: Device_Queue_Info_2_C;
    begin
        DQI2C.Next := Extension_Records.To_C(Struct.Next);
        DQI2C.Flags := Struct.Flags;
        DQI2C.Queue_Family_Index := Struct.Queue_Family_Index;
        DQI2C.Queue_Index := Struct.Queue_Index;

        return DQI2C;
    end To_C;

    procedure Free(Struct: in out Device_Queue_info_2_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Protected_Submit_Info)
        return Protected_Submit_Info_C is
        PSIC: Protected_Submit_Info_C;
    begin
        PSIC.Next := Extension_Records.To_C(Struct.Next);
        PSIC.Protected_Submit := Utilities.To_C(Struct.Protected_Submit);

        return PSIC;
    end To_C;

    procedure Free(Struct: in out Protected_Submit_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Sampler_YCbCr_Conversion_Create_Info)
        return Sampler_YCbCr_Conversion_Create_Info_C is
        SYCCIC: Sampler_YCbCr_Conversion_Create_Info_C;
    begin
        SYCCIC.Next := Extension_Records.To_C(Struct.Next);
        SYCCIC.Format := Struct.Format;
        SYCCIC.YCbCr_Model := Struct.YCbCr_Model;
        SYCCIC.YCbCr_Range := Struct.YCbCr_Range;
        SYCCIC.Components := Struct.Components;
        SYCCIC.X_Chroma_Offset := Struct.X_Chroma_Offset;
        SYCCIC.Y_Chroma_Offset := Struct.Y_Chroma_Offset;
        SYCCIC.Chroma_Filter := Struct.Chroma_Filter;
        SYCCIC.Force_Explicit_Reconstruction :=
            Utilities.To_C(Struct.Force_Explicit_Reconstruction);

        return SYCCIC;
    end To_C;

    procedure Free(Struct: in out Sampler_YCbCr_Conversion_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Sampler_YCbCr_Conversion_Info)
        return Sampler_YCbCr_Conversion_Info_C is
        SYCIC: Sampler_YCbCr_Conversion_Info_C;
    begin
        SYCIC.Next := Extension_Records.To_C(Struct.Next);
        SYCIC.Conversion := Struct.Conversion;

        return SYCIC;
    end To_C;

    procedure Free(Struct: in out Sampler_YCbCr_Conversion_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Bind_Image_Plane_Memory_Info)
        return Bind_Image_Plane_Memory_Info_C is
        BIPMIC: Bind_Image_Plane_Memory_Info_C;
    begin
        BIPMIC.Next := Extension_Records.To_C(Struct.Next);
        BIPMIC.Plane_Aspect := Struct.Plane_Aspect;

        return BIPMIC;
    end To_C;

    procedure Free(Struct: in out Bind_Image_Plane_Memory_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Image_Plane_Memory_Requirements_Info)
        return Image_Plane_Memory_Requirements_Info_C is
        IPMRIC: Image_Plane_Memory_Requirements_Info_C;
    begin
        IPMRIC.Next := Extension_Records.To_C(Struct.Next);
        IPMRIC.Plane_Aspect := Struct.Plane_Aspect;

        return IPMRIC;
    end To_C;

    procedure Free(Struct: in out Image_Plane_Memory_Requirements_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Sampler_YCbCr_Conversion_Features;
         C_Struct: in Physical_Device_Sampler_YCbCr_Conversion_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Sampler_YCbCr_Conversion :=
            Utilities.To_Ada(C_Struct.Sampler_YCbCr_Conversion);
    end To_Ada;

    procedure To_Ada
        (Ada_Struct: in out Sampler_YCbCr_Conversion_Image_Format_Properties;
         C_Struct: in Sampler_YCbCr_Conversion_Image_Format_Properties_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Combined_Image_Sampler_Descriptor_Count :=
            C_Struct.Combined_Image_Sampler_Descriptor_Count;
    end To_Ada;

    function To_C(Struct: in Descriptor_Update_Template_Create_Info)
        return Descriptor_Update_Template_Create_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array
            (Descriptor_Update_Template_Entry_Arrays,
             Descriptor_Update_Template_Entry_Vectors);

        DUTCIC: Descriptor_Update_Template_Create_Info_C;
    begin
        DUTCIC.Next := Extension_Records.To_C(Struct.Next);
        DUTCIC.Flags := Struct.Flags;
        To_C_Array(DUTCIC.Descriptor_Update_Entry_Count,
                   Struct.Descriptor_Update_Entries,
                   DUTCIC.Descriptor_Update_Entries);
        DUTCIC.Template_Type := Struct.Template_Type;
        DUTCIC.Descriptor_Set_Layout := Struct.Descriptor_Set_Layout;
        DUTCIC.Pipeline_Bind_Point := Struct.Pipeline_Bind_Point;
        DUTCIC.Pipeline_Layout := Struct.Pipeline_Layout;
        DUTCIC.Set := Struct.Set;

        return DUTCIC;
    end To_C;

    procedure Free
        (Struct: in out Descriptor_Update_Template_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        Descriptor_Update_Template_Entry_Arrays.Free
            (Struct.Descriptor_Update_Entries);
    end Free;

    function To_C(Struct: in Physical_Device_External_Image_Format_Info)
        return Physical_Device_External_Image_Format_Info_C is
        PDEIFIC: Physical_Device_External_Image_Format_Info_C;
    begin
        PDEIFIC.Next := Extension_Records.To_C(Struct.Next);
        PDEIFIC.Handle_Type := Struct.Handle_Type;

        return PDEIFIC;
    end To_C;

    procedure Free
        (Struct: in out Physical_Device_External_Image_Format_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    procedure To_Ada(Ada_Struct: in out External_Image_Format_Properties;
                     C_Struct: in External_Image_Format_Properties_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.External_Memory_Properties :=
            C_Struct.External_Memory_Properties;
    end To_Ada;

    function To_C(Struct: in Physical_Device_External_Buffer_Info)
        return Physical_Device_External_Buffer_Info_C is
        PDEBIC: Physical_Device_External_Buffer_Info_C;
    begin
        PDEBIC.Next := Extension_Records.To_C(Struct.Next);
        PDEBIC.Flags := Struct.Flags;
        PDEBIC.Usage := Struct.Usage;
        PDEBIC.Handle_Type := Struct.Handle_Type;

        return PDEBIC;
    end To_C;

    procedure Free(Struct: in out Physical_Device_External_Buffer_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    procedure To_Ada(Ada_Struct: in out External_Buffer_Properties;
                     C_Struct: in External_Buffer_Properties_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.External_Memory_Properties :=
            C_Struct.External_Memory_Properties;
    end To_Ada;

    procedure To_Ada(Ada_Struct: in out Physical_Device_ID_Properties;
                     C_Struct: in Physical_Device_ID_Properties_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Device_UUID := C_Struct.Device_UUID;
        Ada_Struct.Driver_UUID := C_Struct.Driver_UUID;
        Ada_Struct.Device_LUID := C_Struct.Device_LUID;
        Ada_Struct.Device_Node_Mask := C_Struct.Device_Node_Mask;
        Ada_Struct.Device_LUID_Valid :=
            Utilities.To_Ada(C_Struct.Device_LUID_Valid);
    end To_Ada;

    function To_C(Struct: in External_Memory_Image_Create_Info)
        return External_Memory_Image_Create_Info_C is
        EMICIC: External_Memory_Image_Create_Info_C;
    begin
        EMICIC.Next := Extension_Records.To_C(Struct.Next);
        EMICIC.Handle_Types := Struct.Handle_Types;

        return EMICIC;
    end To_C;

    procedure Free(Struct: in out External_Memory_Image_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in External_Memory_Buffer_Create_Info)
        return External_Memory_Buffer_Create_Info_C is
        EMBCIC: External_Memory_Buffer_Create_Info_C;
    begin
        EMBCIC.Next := Extension_Records.To_C(Struct.Next);
        EMBCIC.Handle_Types := Struct.Handle_Types;

        return EMBCIC;
    end To_C;

    procedure Free(Struct: in out External_Memory_Buffer_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Export_Memory_Allocate_Info)
        return Export_Memory_Allocate_Info_C is
        EMAIC: Export_Memory_Allocate_Info_C;
    begin
        EMAIC.Next := Extension_Records.To_C(Struct.Next);
        EMAIC.Handle_Types := Struct.Handle_Types;

        return EMAIC;
    end To_C;

    procedure Free(Struct: in out Export_Memory_Allocate_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Physical_Device_External_Fence_Info)
        return Physical_Device_External_Fence_Info_C is
        PDEFIC: Physical_Device_External_Fence_Info_C;
    begin
        PDEFIC.Next := Extension_Records.To_C(Struct.Next);
        PDEFIC.Handle_Type := Struct.Handle_Type;

        return PDEFIC;
    end To_C;

    procedure Free(Struct: in out Physical_Device_External_Fence_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    procedure To_Ada(Ada_Struct: in out External_Fence_Properties;
                     C_Struct: in External_Fence_Properties_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Export_From_Imported_Handle_Types :=
            C_Struct.Export_From_Imported_Handle_Types;
        Ada_Struct.Compatible_Handle_Types := C_Struct.Compatible_Handle_Types;
        Ada_Struct.External_Fence_Features := C_Struct.External_Fence_Features;
    end To_Ada;

    function To_C(Struct: in Export_Fence_Create_Info)
        return Export_Fence_Create_Info_C is
        EFCIC: Export_Fence_Create_Info_C;
    begin
        EFCIC.Next := Extension_Records.To_C(Struct.Next);
        EFCIC.Handle_Types := Struct.Handle_Types;

        return EFCIC;
    end To_C;

    procedure Free(Struct: in out Export_Fence_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Export_Semaphore_Create_Info)
        return Export_Semaphore_Create_Info_C is
        ESCIC: Export_Semaphore_Create_Info_C;
    begin
        ESCIC.Next := Extension_Records.To_C(Struct.Next);
        ESCIC.Handle_Types := Struct.Handle_Types;

        return ESCIC;
    end To_C;

    procedure Free(Struct: in out Export_Semaphore_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Physical_Device_External_Semaphore_Info)
        return Physical_Device_External_Semaphore_Info_C is
        PDESIC: Physical_Device_External_Semaphore_Info_C;
    begin
        PDESIC.Next := Extension_Records.To_C(Struct.Next);
        PDESIC.Handle_Type := Struct.Handle_Type;

        return PDESIC;
    end To_C;

    procedure Free(Struct: in out Physical_Device_External_Semaphore_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    procedure To_Ada(Ada_Struct: in out External_Semaphore_Properties;
                     C_Struct: in External_Semaphore_Properties_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Export_From_Imported_Handle_Types :=
            C_Struct.Export_From_Imported_Handle_Types;
        Ada_Struct.Compatible_Handle_Types := C_Struct.Compatible_Handle_Types;
        Ada_Struct.External_Semaphore_Features :=
            C_Struct.External_Semaphore_Features;
    end To_Ada;

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Maintenance_3_Properties;
         C_Struct: in Physical_Device_Maintenance_3_Properties_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Max_Per_Set_Descriptors := C_Struct.Max_Per_Set_Descriptors;
        Ada_Struct.Max_Memory_Allocation_Size :=
            C_Struct.Max_Memory_Allocation_Size;
    end To_Ada;

    procedure To_Ada(Ada_Struct: in out Descriptor_Set_Layout_Support;
                     C_Struct: in Descriptor_Set_Layout_Support_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Supported := Utilities.To_Ada(C_Struct.Supported);
    end To_Ada;

    procedure To_Ada
        (Ada_Struct: in out Physical_Device_Shader_Draw_Parameter_Features;
         C_Struct: in Physical_Device_Shader_Draw_Parameter_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Shader_Draw_Parameters :=
            Utilities.To_Ada(C_Struct.Shader_Draw_Parameters);
    end To_Ada;

    function To_C(Next: in In_Structure_Access)
        return C.In_Structure_C_Access is
    begin
        if Next = null then
            return null;
        end if;

        case In_Structure(Next.Record_Type) is
            when Bind_Buffer_Memory_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Bind_Buffer_Memory_Info,
                         Bind_Buffer_Memory_Info_C,
                         Bind_Buffer_Memory_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Bind_Image_Memory_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Bind_Image_Memory_Info,
                         Bind_Image_Memory_Info_C,
                         Bind_Image_Memory_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Memory_Dedicated_Allocate_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Memory_Dedicated_Allocate_Info,
                         Memory_Dedicated_Allocate_Info_C,
                         Memory_Dedicated_Allocate_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Memory_Allocate_Flags_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Memory_Allocate_Flags_Info,
                         Memory_Allocate_Flags_Info_C,
                         Memory_Allocate_Flags_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Device_Group_Render_Pass_Begin_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Device_Group_Render_Pass_Begin_Info,
                         Device_Group_Render_Pass_Begin_Info_C,
                         Device_Group_Render_Pass_Begin_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Device_Group_Command_Buffer_Begin_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Device_Group_Command_Buffer_Begin_Info,
                         Device_Group_Command_Buffer_Begin_Info_C,
                         Device_Group_Command_Buffer_Begin_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Device_Group_Submit_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Device_Group_Submit_Info,
                         Device_Group_Submit_Info_C,
                         Device_Group_Submit_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Device_Group_Bind_Sparse_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Device_Group_Bind_Sparse_Info,
                         Device_Group_Bind_Sparse_Info_C,
                         Device_Group_Bind_Sparse_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Bind_Buffer_Memory_Device_Group_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Bind_Buffer_Memory_Device_Group_Info,
                         Bind_Buffer_Memory_Device_Group_Info_C,
                         Bind_Buffer_Memory_Device_Group_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Bind_Image_Memory_Device_Group_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Bind_Image_Memory_Device_Group_Info,
                         Bind_Image_Memory_Device_Group_Info_C,
                         Bind_Image_Memory_Device_Group_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Device_Group_Device_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Device_Group_Device_Create_Info,
                         Device_Group_Device_Create_Info_C,
                         Device_Group_Device_Create_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Buffer_Memory_Requirements_Info_2_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Buffer_Memory_Requirements_Info_2,
                         Buffer_Memory_Requirements_Info_2_C,
                         Buffer_Memory_Requirements_Info_2_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Image_Memory_Requirements_Info_2_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Image_Memory_Requirements_Info_2,
                         Image_Memory_Requirements_Info_2_C,
                         Image_Memory_Requirements_Info_2_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Image_Sparse_Memory_Requirements_Info_2_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Image_Sparse_Memory_Requirements_Info_2,
                         Image_Sparse_Memory_Requirements_Info_2_C,
                         Image_Sparse_Memory_Requirements_Info_2_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Image_Format_Info_2_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Physical_Device_Image_Format_Info_2,
                         Physical_Device_Image_Format_Info_2_C,
                         Physical_Device_Image_Format_Info_2_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Sparse_Image_Format_Info_2_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Physical_Device_Sparse_Image_Format_Info_2,
                         Physical_Device_Sparse_Image_Format_Info_2_C,
                         Physical_Device_Sparse_Image_Format_Info_2_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Render_Pass_Input_Attachment_Aspect_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                   (Render_Pass_Input_Attachment_Aspect_Create_Info,
                    Render_Pass_Input_Attachment_Aspect_Create_Info_C,
                    Render_Pass_Input_Attachment_Aspect_Create_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Image_View_Usage_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Image_View_Usage_Create_Info,
                         Image_View_Usage_Create_Info_C,
                         Image_View_Usage_Create_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Pipeline_Tessellation_Domain_Origin_State_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
               (Pipeline_Tessellation_Domain_Origin_State_Create_Info,
                Pipeline_Tessellation_Domain_Origin_State_Create_Info_C,
                Pipeline_Tessellation_Domain_Origin_State_Create_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Render_Pass_Multiview_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Render_Pass_Multiview_Create_Info,
                         Render_Pass_Multiview_Create_Info_C,
                         Render_Pass_Multiview_Create_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Device_Queue_Info_2_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Device_Queue_Info_2,
                         Device_Queue_Info_2_C,
                         Device_Queue_Info_2_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Protected_Submit_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Protected_Submit_Info,
                         Protected_Submit_Info_C,
                         Protected_Submit_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Sampler_YCbCr_Conversion_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Sampler_YCbCr_Conversion_Create_Info,
                         Sampler_YCbCr_Conversion_Create_Info_C,
                         Sampler_YCbCr_Conversion_Create_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Sampler_YCbCr_Conversion_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Sampler_YCbCr_Conversion_Info,
                         Sampler_YCbCr_Conversion_Info_C,
                         Sampler_YCbCr_Conversion_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Bind_Image_Plane_Memory_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Bind_Image_Plane_Memory_Info,
                         Bind_Image_Plane_Memory_Info_C,
                         Bind_Image_Plane_Memory_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Image_Plane_Memory_Requirements_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Image_Plane_Memory_Requirements_Info,
                         Image_Plane_Memory_Requirements_Info_C,
                         Image_Plane_Memory_Requirements_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Descriptor_Update_Template_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Descriptor_Update_Template_Create_Info,
                         Descriptor_Update_Template_Create_Info_C,
                         Descriptor_Update_Template_Create_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_External_Image_Format_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Physical_Device_External_Image_Format_Info,
                        Physical_Device_External_Image_Format_Info_C,
                        Physical_Device_External_Image_Format_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_External_Buffer_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Physical_Device_External_Buffer_Info,
                         Physical_Device_External_Buffer_Info_C,
                         Physical_Device_External_Buffer_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when External_Memory_Image_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (External_Memory_Image_Create_Info,
                         External_Memory_Image_Create_Info_C,
                         External_Memory_Image_Create_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when External_Memory_Buffer_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (External_Memory_Buffer_Create_Info,
                         External_Memory_Buffer_Create_Info_C,
                         External_Memory_Buffer_Create_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Export_Memory_Allocate_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Export_Memory_Allocate_Info,
                         Export_Memory_Allocate_Info_C,
                         Export_Memory_Allocate_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_External_Fence_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Physical_Device_External_Fence_Info,
                         Physical_Device_External_Fence_Info_C,
                         Physical_Device_External_Fence_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Export_Fence_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Export_Fence_Create_Info,
                         Export_Fence_Create_Info_C,
                         Export_Fence_Create_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Export_Semaphore_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Export_Semaphore_Create_Info,
                         Export_Semaphore_Create_Info_C,
                         Export_Semaphore_Create_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_External_Semaphore_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Physical_Device_External_Semaphore_Info,
                         Physical_Device_External_Semaphore_Info_C,
                         Physical_Device_External_Semaphore_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
        end case;
    end To_C;

    function To_C(Next: in Out_Structure_Access)
        return C.Out_Structure_C_Access is
    begin
        if Next = null then
            return null;
        end if;

        case Out_Structure(Next.Record_Type) is
            when Physical_Device_Subgroup_Properties_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Physical_Device_Subgroup_Properties,
                         Physical_Device_Subgroup_Properties_C,
                         Physical_Device_Subgroup_Properties_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_16Bit_Storage_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Physical_Device_16Bit_Storage_Features,
                         Physical_Device_16Bit_Storage_Features_C,
                         Physical_Device_16Bit_Storage_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Memory_Dedicated_Requirements_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Memory_Dedicated_Requirements,
                         Memory_Dedicated_Requirements_C,
                         Memory_Dedicated_Requirements_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Group_Properties_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Physical_Device_Group_Properties,
                         Physical_Device_Group_Properties_C,
                         Physical_Device_Group_Properties_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Memory_Requirements_2_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Memory_Requirements_2,
                         Memory_Requirements_2_C,
                         Memory_Requirements_2_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Sparse_Image_Memory_Requirements_2_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Sparse_Image_Memory_Requirements_2,
                         Sparse_Image_Memory_Requirements_2_C,
                         Sparse_Image_Memory_Requirements_2_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Features_2_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Physical_Device_Features_2,
                         Physical_Device_Features_2_C,
                         Physical_Device_Features_2_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Properties_2_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Physical_Device_Properties_2,
                         Physical_Device_Properties_2_C,
                         Physical_Device_Properties_2_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Format_Properties_2_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Format_Properties_2,
                         Format_Properties_2_C,
                         Format_Properties_2_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Image_Format_Properties_2_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Image_Format_Properties_2,
                         Image_Format_Properties_2_C,
                         Image_Format_Properties_2_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Queue_Family_Properties_2_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Queue_Family_Properties_2,
                         Queue_Family_Properties_2_C,
                         Queue_Family_Properties_2_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Memory_Properties_2_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Physical_Device_Memory_Properties_2,
                         Physical_Device_Memory_Properties_2_C,
                         Physical_Device_Memory_Properties_2_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Sparse_Image_Format_Properties_2_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Sparse_Image_Format_Properties_2,
                         Sparse_Image_Format_Properties_2_C,
                         Sparse_Image_Format_Properties_2_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Point_Clipping_Properties_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Physical_Device_Point_Clipping_Properties,
                         Physical_Device_Point_Clipping_Properties_C,
                         Physical_Device_Point_Clipping_Properties_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Multiview_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Physical_Device_Multiview_Features,
                         Physical_Device_Multiview_Features_C,
                         Physical_Device_Multiview_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Multiview_Properties_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Physical_Device_Multiview_Properties,
                         Physical_Device_Multiview_Properties_C,
                         Physical_Device_Multiview_Properties_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Variable_Pointer_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Physical_Device_Variable_Pointer_Features,
                         Physical_Device_Variable_Pointer_Features_C,
                         Physical_Device_Variable_Pointer_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Protected_Memory_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Physical_Device_Protected_Memory_Features,
                         Physical_Device_Protected_Memory_Features_C,
                         Physical_Device_Protected_Memory_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Protected_Memory_Properties_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                       (Physical_Device_Protected_Memory_Properties,
                        Physical_Device_Protected_Memory_Properties_C,
                        Physical_Device_Protected_Memory_Properties_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Sampler_YCbCr_Conversion_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                   (Physical_Device_Sampler_YCbCr_Conversion_Features,
                    Physical_Device_Sampler_YCbCr_Conversion_Features_C,
                    Physical_Device_Sampler_YCbCr_Conversion_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Sampler_YCbCr_Conversion_Image_Format_Properties_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                    (Sampler_YCbCr_Conversion_Image_Format_Properties,
                     Sampler_YCbCr_Conversion_Image_Format_Properties_C,
                     Sampler_YCbCr_Conversion_Image_Format_Properties_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when External_Image_Format_Properties_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (External_Image_Format_Properties,
                         External_Image_Format_Properties_C,
                         External_Image_Format_Properties_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when External_Buffer_Properties_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (External_Buffer_Properties,
                         External_Buffer_Properties_C,
                         External_Buffer_Properties_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_ID_Properties_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Physical_Device_ID_Properties,
                         Physical_Device_ID_Properties_C,
                         Physical_Device_ID_Properties_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when External_Fence_Properties_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (External_Fence_Properties,
                         External_Fence_Properties_C,
                         External_Fence_Properties_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when External_Semaphore_Properties_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (External_Semaphore_Properties,
                         External_Semaphore_Properties_C,
                         External_Semaphore_Properties_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Maintenance_3_Properties_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Physical_Device_Maintenance_3_Properties,
                         Physical_Device_Maintenance_3_Properties_C,
                         Physical_Device_Maintenance_3_Properties_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Descriptor_Set_Layout_Support_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Descriptor_Set_Layout_Support,
                         Descriptor_Set_Layout_Support_C,
                         Descriptor_Set_Layout_Support_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Shader_Draw_Parameter_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                    (Physical_Device_Shader_Draw_Parameter_Features,
                     Physical_Device_Shader_Draw_Parameter_Features_C,
                     Physical_Device_Shader_Draw_Parameter_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
        end case;
    end To_C;

    procedure To_Ada(Ada_Struct: in out Vulkan.Out_Structure'Class;
                     Next: in C.Out_Structure_C_Access) is
    begin
        case Out_Structure(Ada_Struct.Record_Type) is
            when Physical_Device_Subgroup_Properties_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Physical_Device_Subgroup_Properties_C_Access);
                begin
                    To_Ada(Physical_Device_Subgroup_Properties(Ada_Struct),
                           To_Access(Next).all);
                end;
            when Physical_Device_16Bit_Storage_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Physical_Device_16Bit_Storage_Features_C_Access);
                begin
                    To_Ada(Physical_Device_16Bit_Storage_Features(Ada_Struct),
                           To_Access(Next).all);
                end;
            when Memory_Dedicated_Requirements_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Memory_Dedicated_Requirements_C_Access);
                begin
                    To_Ada(Memory_Dedicated_Requirements(Ada_Struct),
                           To_Access(Next).all);
                end;
            when Physical_Device_Group_Properties_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Physical_Device_Group_Properties_C_Access);
                begin
                    To_Ada(Physical_Device_Group_Properties(Ada_Struct),
                           To_Access(Next).all);
                end;
            when Memory_Requirements_2_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Memory_Requirements_2_C_Access);
                begin
                    To_Ada(Memory_Requirements_2(Ada_Struct),
                           To_Access(Next).all);
                end;
            when Sparse_Image_Memory_Requirements_2_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Sparse_Image_Memory_Requirements_2_C_Access);
                begin
                    To_Ada(Sparse_Image_Memory_Requirements_2(Ada_Struct),
                           To_Access(Next).all);
                end;
            when Physical_Device_Features_2_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Physical_Device_Features_2_C_Access);
                begin
                    To_Ada(Physical_Device_Features_2(Ada_Struct),
                           To_Access(Next).all);
                end;
            when Physical_Device_Properties_2_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Physical_Device_Properties_2_C_Access);
                begin
                    To_Ada(Physical_Device_Properties_2(Ada_Struct),
                           To_Access(Next).all);
                end;
            when Format_Properties_2_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Format_Properties_2_C_Access);
                begin
                    To_Ada(Format_Properties_2(Ada_Struct),
                           To_Access(Next).all);
                end;
            when Image_Format_Properties_2_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Image_Format_Properties_2_C_Access);
                begin
                    To_Ada(Image_Format_Properties_2(Ada_Struct),
                           To_Access(Next).all);
                end;
            when Queue_Family_Properties_2_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Queue_Family_Properties_2_C_Access);
                begin
                    To_Ada(Queue_Family_Properties_2(Ada_Struct),
                           To_Access(Next).all);
                end;
            when Physical_Device_Memory_Properties_2_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Physical_Device_Memory_Properties_2_C_Access);
                begin
                    To_Ada(Physical_Device_Memory_Properties_2(Ada_Struct),
                           To_Access(Next).all);
                end;
            when Sparse_Image_Format_Properties_2_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Sparse_Image_Format_Properties_2_C_Access);
                begin
                    To_Ada(Sparse_Image_Format_Properties_2(Ada_Struct),
                           To_Access(Next).all);
                end;
            when Physical_Device_Point_Clipping_Properties_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Physical_Device_Point_Clipping_Properties_C_Access);
                begin
                    To_Ada
                        (Physical_Device_Point_Clipping_Properties(Ada_Struct),
                         To_Access(Next).all);
                end;
            when Physical_Device_Multiview_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Physical_Device_Multiview_Features_C_Access);
                begin
                    To_Ada(Physical_Device_Multiview_Features(Ada_Struct),
                           To_Access(Next).all);
                end;
            when Physical_Device_Multiview_Properties_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Physical_Device_Multiview_Properties_C_Access);
                begin
                    To_Ada(Physical_Device_Multiview_Properties(Ada_Struct),
                           To_Access(Next).all);
                end;
            when Physical_Device_Variable_Pointer_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Physical_Device_Variable_Pointer_Features_C_Access);
                begin
                    To_Ada
                        (Physical_Device_Variable_Pointer_Features(Ada_Struct),
                         To_Access(Next).all);
                end;
            when Physical_Device_Protected_Memory_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Physical_Device_Protected_Memory_Features_C_Access);
                begin
                    To_Ada
                        (Physical_Device_Protected_Memory_Features(Ada_Struct),
                         To_Access(Next).all);
                end;
            when Physical_Device_Protected_Memory_Properties_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Physical_Device_Protected_Memory_Properties_C_Access);
                begin
                    To_Ada
                       (Physical_Device_Protected_Memory_Properties(Ada_Struct),
                        To_Access(Next).all);
                end;
            when Physical_Device_Sampler_YCbCr_Conversion_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                   (C.Out_Structure_C_Access,
                    Physical_Device_Sampler_YCbCr_Conversion_Features_C_Access);
                begin
                    To_Ada
                 (Physical_Device_Sampler_YCbCr_Conversion_Features(Ada_Struct),
                  To_Access(Next).all);
                end;
            when Sampler_YCbCr_Conversion_Image_Format_Properties_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                    (C.Out_Structure_C_Access,
                     Sampler_YCbCr_Conversion_Image_Format_Properties_C_Access);
                begin
                    To_Ada
                  (Sampler_YCbCr_Conversion_Image_Format_Properties(Ada_Struct),
                   To_Access(Next).all);
                end;
            when External_Image_Format_Properties_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         External_Image_Format_Properties_C_Access);
                begin
                    To_Ada(External_Image_Format_Properties(Ada_Struct),
                           To_Access(Next).all);
                end;
            when External_Buffer_Properties_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         External_Buffer_Properties_C_Access);
                begin
                    To_Ada(External_Buffer_Properties(Ada_Struct),
                           To_Access(Next).all);
                end;
            when Physical_Device_ID_Properties_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Physical_Device_ID_Properties_C_Access);
                begin
                    To_Ada(Physical_Device_ID_Properties(Ada_Struct),
                           To_Access(Next).all);
                end;
            when External_Fence_Properties_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         External_Fence_Properties_C_Access);
                begin
                    To_Ada(External_Fence_Properties(Ada_Struct),
                           To_Access(Next).all);
                end;
            when External_Semaphore_Properties_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         External_Semaphore_Properties_C_Access);
                begin
                    To_Ada(External_Semaphore_Properties(Ada_Struct),
                           To_Access(Next).all);
                end;
            when Physical_Device_Maintenance_3_Properties_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Physical_Device_Maintenance_3_Properties_C_Access);
                begin
                    To_Ada(Physical_Device_Maintenance_3_Properties(Ada_Struct),
                           To_Access(Next).all);
                end;
            when Descriptor_Set_Layout_Support_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Descriptor_Set_Layout_Support_C_Access);
                begin
                    To_Ada(Descriptor_Set_Layout_Support(Ada_Struct),
                           To_Access(Next).all);
                end;
            when Physical_Device_Shader_Draw_Parameter_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                      (C.Out_Structure_C_Access,
                       Physical_Device_Shader_Draw_Parameter_Features_C_Access);
                begin
                    To_Ada
                    (Physical_Device_Shader_Draw_Parameter_Features(Ada_Struct),
                     To_Access(Next).all);
                end;
        end case;
    end To_Ada;

    procedure Free(Next: in out C.In_Structure_C_Access) is
        use type C.In_Structure_C_Access;
    begin
        if Next = null then
            return;
        end if;

        case In_Structure(Next.Record_Type) is
            when Bind_Buffer_Memory_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Bind_Buffer_Memory_Info_C,
                         Bind_Buffer_Memory_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Bind_Image_Memory_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Bind_Image_Memory_Info_C,
                         Bind_Image_Memory_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Memory_Dedicated_Allocate_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Memory_Dedicated_Allocate_Info_C,
                         Memory_Dedicated_Allocate_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Memory_Allocate_Flags_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Memory_Allocate_Flags_Info_C,
                         Memory_Allocate_Flags_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Device_Group_Render_Pass_Begin_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Device_Group_Render_Pass_Begin_Info_C,
                         Device_Group_Render_Pass_Begin_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Device_Group_Command_Buffer_Begin_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Device_Group_Command_Buffer_Begin_Info_C,
                         Device_Group_Command_Buffer_Begin_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Device_Group_Submit_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Device_Group_Submit_Info_C,
                         Device_Group_Submit_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Device_Group_Bind_Sparse_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Device_Group_Bind_Sparse_Info_C,
                         Device_Group_Bind_Sparse_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Bind_Buffer_Memory_Device_Group_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Bind_Buffer_Memory_Device_Group_Info_C,
                         Bind_Buffer_Memory_Device_Group_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Bind_Image_Memory_Device_Group_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Bind_Image_Memory_Device_Group_Info_C,
                         Bind_Image_Memory_Device_Group_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Device_Group_Device_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Device_Group_Device_Create_Info_C,
                         Device_Group_Device_Create_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Buffer_Memory_Requirements_Info_2_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Buffer_Memory_Requirements_Info_2_C,
                         Buffer_Memory_Requirements_Info_2_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Image_Memory_Requirements_Info_2_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Image_Memory_Requirements_Info_2_C,
                         Image_Memory_Requirements_Info_2_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Image_Sparse_Memory_Requirements_Info_2_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Image_Sparse_Memory_Requirements_Info_2_C,
                         Image_Sparse_Memory_Requirements_Info_2_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Image_Format_Info_2_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Physical_Device_Image_Format_Info_2_C,
                         Physical_Device_Image_Format_Info_2_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Sparse_Image_Format_Info_2_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Physical_Device_Sparse_Image_Format_Info_2_C,
                         Physical_Device_Sparse_Image_Format_Info_2_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Render_Pass_Input_Attachment_Aspect_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                     (Render_Pass_Input_Attachment_Aspect_Create_Info_C,
                      Render_Pass_Input_Attachment_Aspect_Create_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Image_View_Usage_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Image_View_Usage_Create_Info_C,
                         Image_View_Usage_Create_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when
               Pipeline_Tessellation_Domain_Origin_State_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
               (Pipeline_Tessellation_Domain_Origin_State_Create_Info_C,
                Pipeline_Tessellation_Domain_Origin_State_Create_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Render_Pass_Multiview_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Render_Pass_Multiview_Create_Info_C,
                         Render_Pass_Multiview_Create_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Device_Queue_Info_2_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Device_Queue_Info_2_C, Device_Queue_Info_2_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Protected_Submit_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Protected_Submit_Info_C,
                         Protected_Submit_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Sampler_YCbCr_Conversion_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Sampler_YCbCr_Conversion_Create_Info_C,
                         Sampler_YCbCr_Conversion_Create_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Sampler_YCbCr_Conversion_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Sampler_YCbCr_Conversion_Info_C,
                         Sampler_YCbCr_Conversion_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Bind_Image_Plane_Memory_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Bind_Image_Plane_Memory_Info_C,
                         Bind_Image_Plane_Memory_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Image_Plane_Memory_Requirements_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Image_Plane_Memory_Requirements_Info_C,
                         Image_Plane_Memory_Requirements_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Descriptor_Update_Template_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Descriptor_Update_Template_Create_Info_C,
                         Descriptor_Update_Template_Create_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_External_Image_Format_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Physical_Device_External_Image_Format_Info_C,
                         Physical_Device_External_Image_Format_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_External_Buffer_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Physical_Device_External_Buffer_Info_C,
                         Physical_Device_External_Buffer_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when External_Memory_Image_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (External_Memory_Image_Create_Info_C,
                         External_Memory_Image_Create_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when External_Memory_Buffer_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (External_Memory_Buffer_Create_Info_C,
                         External_Memory_Buffer_Create_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Export_Memory_Allocate_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Export_Memory_Allocate_Info_C,
                         Export_Memory_Allocate_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_External_Fence_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Physical_Device_External_Fence_Info_C,
                         Physical_Device_External_Fence_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Export_Fence_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Export_Fence_Create_Info_C,
                         Export_Fence_Create_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Export_Semaphore_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Export_Semaphore_Create_Info_C,
                         Export_Semaphore_Create_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_External_Semaphore_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Physical_Device_External_Semaphore_Info_C,
                         Physical_Device_External_Semaphore_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
        end case;
    end Free;

    procedure Free(Next: in out C.Out_Structure_C_Access) is
        use type C.Out_Structure_C_Access;
    begin
        if Next = null then
            return;
        end if;

        case Out_Structure(Next.Record_Type) is
            when Physical_Device_Subgroup_Properties_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Physical_Device_Subgroup_Properties_C,
                         Physical_Device_Subgroup_Properties_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_16Bit_Storage_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Physical_Device_16Bit_Storage_Features_C,
                         Physical_Device_16Bit_Storage_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Memory_Dedicated_Requirements_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Memory_Dedicated_Requirements_C,
                         Memory_Dedicated_Requirements_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Group_Properties_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Physical_Device_Group_Properties_C,
                         Physical_Device_Group_Properties_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Memory_Requirements_2_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Memory_Requirements_2_C,
                         Memory_Requirements_2_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Sparse_Image_Memory_Requirements_2_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Sparse_Image_Memory_Requirements_2_C,
                         Sparse_Image_Memory_Requirements_2_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Features_2_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Physical_Device_Features_2_C,
                         Physical_Device_Features_2_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Properties_2_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Physical_Device_Properties_2_C,
                         Physical_Device_Properties_2_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Format_Properties_2_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Format_Properties_2_C,
                         Format_Properties_2_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Image_Format_Properties_2_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Image_Format_Properties_2_C,
                         Image_Format_Properties_2_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Queue_Family_Properties_2_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Queue_Family_Properties_2_C,
                         Queue_Family_Properties_2_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Memory_Properties_2_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Physical_Device_Memory_Properties_2_C,
                         Physical_Device_Memory_Properties_2_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Sparse_Image_Format_Properties_2_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Sparse_Image_Format_Properties_2_C,
                         Sparse_Image_Format_Properties_2_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Point_Clipping_Properties_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Physical_Device_Point_Clipping_Properties_C,
                         Physical_Device_Point_Clipping_Properties_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Multiview_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Physical_Device_Multiview_Features_C,
                         Physical_Device_Multiview_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Multiview_Properties_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Physical_Device_Multiview_Properties_C,
                         Physical_Device_Multiview_Properties_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Variable_Pointer_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Physical_Device_Variable_Pointer_Features_C,
                         Physical_Device_Variable_Pointer_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Protected_Memory_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Physical_Device_Protected_Memory_Features_C,
                         Physical_Device_Protected_Memory_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Protected_Memory_Properties_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Physical_Device_Protected_Memory_Properties_C,
                         Physical_Device_Protected_Memory_Properties_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Sampler_YCbCr_Conversion_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                   (Physical_Device_Sampler_YCbCr_Conversion_Features_C,
                    Physical_Device_Sampler_YCbCr_Conversion_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Sampler_YCbCr_Conversion_Image_Format_Properties_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                    (Sampler_YCbCr_Conversion_Image_Format_Properties_C,
                     Sampler_YCbCr_Conversion_Image_Format_Properties_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when External_Image_Format_Properties_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (External_Image_Format_Properties_C,
                         External_Image_Format_Properties_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when External_Buffer_Properties_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (External_Buffer_Properties_C,
                         External_Buffer_Properties_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_ID_Properties_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Physical_Device_ID_Properties_C,
                         Physical_Device_ID_Properties_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when External_Fence_Properties_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (External_Fence_Properties_C,
                         External_Fence_Properties_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when External_Semaphore_Properties_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (External_Semaphore_Properties_C,
                         External_Semaphore_Properties_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Maintenance_3_Properties_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Physical_Device_Maintenance_3_Properties_C,
                         Physical_Device_Maintenance_3_Properties_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Descriptor_Set_Layout_Support_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Descriptor_Set_Layout_Support_C,
                         Descriptor_Set_Layout_Support_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Shader_Draw_Parameter_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                      (Physical_Device_Shader_Draw_Parameter_Features_C,
                       Physical_Device_Shader_Draw_Parameter_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
        end case;
    end Free;

begin
    declare
        function Load is
            new Core.Get_Proc_Addr(vkEnumerateInstanceVersion_Access);
    begin
        vkEnumerateInstanceVersion := Load(No_Instance,
                                           "vkEnumerateInstanceVersion");
    end;
end Vulkan.C_V1_1;

