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

-- C interface for KHR records

with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;
with Vulkan.Extension_Records;
with Vulkan.Utilities;

package body Vulkan.C_KHR is
    function To_C(Struct: in Extensions.KHR.Swapchain_Create_Info)
        return Swapchain_Create_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array
            (C.Queue_Family_Index_Arrays, Queue_Family_Index_Vectors);

        SCIC: Swapchain_Create_Info_C;
    begin
        SCIC.Next := Extension_Records.To_C(Struct.Next);
        SCIC.Flags := Struct.Flags;
        SCIC.Surface := Struct.Surface;
        SCIC.Min_Image_Count := Struct.Min_Image_Count;
        SCIC.Image_Format := Struct.Image_Format;
        SCIC.Image_Color_Space := Struct.Image_Color_Space;
        SCIC.Image_Extent := Struct.Image_Extent;
        SCIC.Image_Array_Layers := Struct.Image_Array_Layers;
        SCIC.Image_Usage := Struct.Image_Usage;
        SCIC.Image_Sharing_Mode := Struct.Image_Sharing_Mode;
        To_C_Array(SCIC.Queue_Family_Index_Count,
                   Struct.Queue_Family_Indices,
                   SCIC.Queue_Family_Indices);
        SCIC.Pre_Transform := Struct.Pre_Transform;
        SCIC.Composite_Alpha := Struct.Composite_Alpha;
        SCIC.Present_Mode := Struct.Present_Mode;
        SCIC.Clipped := Utilities.To_C(Struct.Clipped);
        SCIC.Old_Swapchain := Struct.Old_Swapchain;

        return SCIC;
    end To_C;

    procedure Free(Struct: in out Swapchain_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        C.Queue_Family_Index_Arrays.Free(Struct.Queue_Family_Indices);
    end Free;

    function To_C(Struct: in Extensions.KHR.Present_Info)
        return Present_Info_C is
        procedure To_C_Array is
            new Utilities.To_C_Array(C.Semaphore_Arrays, Semaphore_Vectors);
        procedure To_C_Array is
            new Utilities.To_C_Array(Swapchain_Arrays,
                                     Extensions.KHR.Swapchain_Vectors);
        procedure To_C_Array is new Utilities.To_C_Array(C.Uint32_t_Arrays,
                                                         Unsigned_32_Vectors);
        procedure To_C_Array is new Utilities.To_C_Array(Result_Arrays,
                                                         Result_Vectors);

        PIC: Present_Info_C;
        Dummy: Interfaces.Unsigned_32;
    begin
        PIC.Next := Extension_Records.To_C(Struct.Next);
        To_C_Array(PIC.Wait_Semaphore_Count,
                   Struct.Wait_Semaphores,
                   PIC.Wait_Semaphores);
        To_C_Array(PIC.Swapchain_Count,
                   Struct.Swapchains,
                   PIC.Swapchains);
        To_C_Array(Dummy,
                   Struct.Image_Indices,
                   PIC.Image_Indices);
        To_C_Array(Dummy,
                   Struct.Results,
                   PIC.Results);

        return PIC;
    end To_C;

    procedure Free(Struct: in out Present_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        C.Semaphore_Arrays.Free(Struct.Wait_Semaphores);
        Swapchain_Arrays.Free(Struct.Swapchains);
        C.Uint32_t_Arrays.Free(Struct.Image_Indices);
        Result_Arrays.Free(Struct.Results);
    end Free;

    function To_C(Struct: in Extensions.KHR.Image_Swapchain_Create_Info)
        return Image_Swapchain_Create_Info_C is
        ISCIC: Image_Swapchain_Create_Info_C;
    begin
        ISCIC.Next := Extension_Records.To_C(Struct.Next);
        ISCIC.Swapchain := Struct.Swapchain;

        return ISCIC;
    end To_C;

    procedure Free(Struct: in out Image_Swapchain_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Extensions.KHR.Bind_Image_Memory_Swapchain_Info)
        return Bind_Image_Memory_Swapchain_Info_C is
        BIMSIC: Bind_Image_Memory_Swapchain_Info_C;
    begin
        BIMSIC.Next := Extension_Records.To_C(Struct.Next);
        BIMSIC.Swapchain := Struct.Swapchain;
        BIMSIC.Image_Index := Struct.Image_Index;

        return BIMSIC;
    end To_C;

    procedure Free(Struct: in out Bind_Image_Memory_Swapchain_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Extensions.KHR.Acquire_Next_Image_Info)
        return Acquire_Next_Image_Info_C is
        ANIIC: Acquire_Next_Image_Info_C;
    begin
        ANIIC.Next := Extension_Records.To_C(Struct.Next);
        ANIIC.Swapchain := Struct.Swapchain;
        ANIIC.Timeout := Struct.Timeout;
        ANIIC.Semaphore := Struct.Semaphore;
        ANIIC.Fence := Struct.Fence;
        ANIIC.Device_Mask := Struct.Device_Mask;

        return ANIIC;
    end To_C;

    procedure Free(Struct: in out Acquire_Next_Image_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    procedure To_Ada
        (Ada_Struct: in out Extensions.KHR.Device_Group_Present_Capabilities;
         C_Struct: in Device_Group_Present_Capabilities_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Present_Mask := C_Struct.Present_Mask;
        Ada_Struct.Modes := C_Struct.Modes;
    end To_Ada;

    function To_C(Struct: in Extensions.KHR.Device_Group_Present_Info)
        return Device_Group_Present_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array(C.Uint32_t_Arrays,
                                                         Unsigned_32_Vectors);

        DGPIC: Device_Group_Present_Info_C;
    begin
        DGPIC.Next := Extension_Records.To_C(Struct.Next);
        To_C_Array(DGPIC.Swapchain_Count,
                   Struct.Device_Masks,
                   DGPIC.Device_Masks);
        DGPIC.Mode := Struct.Mode;

        return DGPIC;
    end To_C;

    procedure Free(Struct: in out Device_Group_Present_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        C.Uint32_t_Arrays.Free(Struct.Device_Masks);
    end Free;

    function To_C(Struct: in Extensions.KHR.Device_Group_Swapchain_Create_Info)
        return Device_Group_Swapchain_Create_Info_C is
        DGWCIC: Device_Group_Swapchain_Create_Info_C;
    begin
        DGWCIC.Next := Extension_Records.To_C(Struct.Next);
        DGWCIC.Modes := Struct.Modes;

        return DGWCIC;
    end To_C;

    procedure Free(Struct: in out Device_Group_Swapchain_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free; 

    function To_C(Struct: in Extensions.KHR.Display_Mode_Create_Info)
        return Display_Mode_Create_Info_C is
        DMCIC: Display_Mode_Create_Info_C;
    begin
        DMCIC.Next := Extension_Records.To_C(Struct.Next);
        DMCIC.Flags := Struct.Flags;
        DMCIC.Parameters := Struct.Parameters;

        return DMCIC;
    end To_C;

    procedure Free(Struct: in out Display_Mode_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Extensions.KHR.Display_Properties)
        return Display_Properties_C is
        use type Ada.Strings.Unbounded.Unbounded_String;

        DPC: Display_Properties_C;
    begin
        DPC.Display := Struct.Display;

        if Struct.Display_Name /= "" then
            DPC.Display_Name := Interfaces.C.Strings.New_String
                (Ada.Strings.Unbounded.To_String(Struct.Display_Name));
        end if;

        DPC.Physical_Dimensions := Struct.Physical_Dimensions;
        DPC.Physical_Resolution := Struct.Physical_Resolution;
        DPC.Supported_Transforms := Struct.Supported_Transforms;
        DPC.Plane_Reorder_Possible :=
            Utilities.To_C(Struct.Plane_Reorder_Possible);
        DPC.Persistent_Content := Utilities.To_C(Struct.Persistent_Content);

        return DPC;
    end To_C;

    function To_Ada(DPC: Display_Properties_C) return
        Extensions.KHR.Display_Properties is
        use type Interfaces.Unsigned_32;
        use type Interfaces.C.Strings.chars_ptr;

        DP: Extensions.KHR.Display_Properties;
    begin
        DP.Display := DPC.Display;

        if DPC.Display_Name /= Interfaces.C.Strings.Null_Ptr then
            DP.Display_Name := Ada.Strings.Unbounded.To_Unbounded_String
                                (Interfaces.C.Strings.Value(DPC.Display_Name));
        end if;

        DP.Physical_Dimensions := DPC.Physical_Dimensions;
        DP.Physical_Resolution := DPC.Physical_Resolution;
        DP.Supported_Transforms := DPC.Supported_Transforms;
        DP.Plane_Reorder_Possible := DPC.Plane_Reorder_Possible /= 0;
        DP.Persistent_Content := DPC.Persistent_Content /= 0;

        return DP;
    end To_Ada;

    procedure Free(Struct: in out Display_Properties_C) is
    begin
        Interfaces.C.Strings.Free(Struct.Display_Name);
    end Free;

    function To_C(Struct: in Extensions.KHR.Display_Surface_Create_Info)
        return Display_Surface_Create_Info_C is
        DSCIC: Display_Surface_Create_Info_C;
    begin
        DSCIC.Next := Extension_Records.To_C(Struct.Next);
        DSCIC.Flags := Struct.Flags;
        DSCIC.Display_Mode := Struct.Display_Mode;
        DSCIC.Plane_Index := Struct.Plane_Index;
        DSCIC.Plane_Stack_Index := Struct.Plane_Stack_Index;
        DSCIC.Transform := Struct.Transform;
        DSCIC.Global_Alpha := Interfaces.C.C_float(Struct.Global_Alpha);
        DSCIC.Alpha_Mode := Struct.Alpha_Mode;
        DSCIC.Image_Extent := Struct.Image_Extent;

        return DSCIC;
    end To_C;

    procedure Free(Struct: in out Display_Surface_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Extensions.KHR.Display_Present_Info)
        return Display_Present_Info_C is
        DPIC: Display_Present_Info_C;
    begin
        DPIC.Next := Extension_Records.To_C(Struct.Next);
        DPIC.Src_Rect := Struct.Src_Rect;
        DPIC.Dst_Rect := Struct.Dst_Rect;
        DPIC.Persistent := Utilities.To_C(Struct.Persistent);

        return DPIC;
    end To_C;

    procedure Free(Struct: in out Display_Present_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    procedure To_Ada
        (Ada_Struct:
            in out Extensions.KHR.Queue_Family_Query_Result_Status_Properties;
         C_Struct: in Queue_Family_Query_Result_Status_Properties_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Query_Result_Status_Support :=
            Utilities.To_Ada(C_Struct.Query_Result_Status_Support);
    end To_Ada;

    procedure To_Ada
        (Ada_Struct: in out Extensions.KHR.Queue_Family_Video_Properties;
         C_Struct: in Queue_Family_Video_Properties_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Video_Codec_Operations := C_Struct.Video_Codec_Operations;
    end To_Ada;

    function To_C(Struct: in Extensions.KHR.Video_Profile_Info)
        return Video_Profile_Info_C is
        VPIC: Video_Profile_Info_C;
    begin
        VPIC.Next := Extension_Records.To_C(Struct.Next);
        VPIC.Video_Codec_Operation := Struct.Video_Codec_Operation;
        VPIC.Chroma_Subsampling := Struct.Chroma_Subsampling;
        VPIC.Luma_Bit_Depth := Struct.Luma_Bit_Depth;
        VPIC.Chroma_Bit_Depth := Struct.Chroma_Bit_Depth;

        return VPIC;
    end To_C;

    procedure Free(Struct: in out Video_Profile_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Extensions.KHR.Video_Profile_List_Info)
        return Video_Profile_List_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array_Convert
            (Video_Profile_Info_C_Arrays,
             Extensions.KHR.Video_Profile_Info_Vectors);

        VPLIC: Video_Profile_List_Info_C;
    begin
        VPLIC.Next := Extension_Records.To_C(Struct.Next);
        To_C_Array(VPLIC.Profile_Count,
                   Struct.Profiles,
                   VPLIC.Profiles);

        return VPLIC;
    end To_C;

    procedure Free(Struct: in out Video_Profile_List_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        Video_Profile_Info_C_Arrays.Free(Struct.Profiles, Free'Access);
    end Free;

    procedure To_Ada(Ada_Struct: in out Extensions.KHR.Video_Capabilities;
                     C_Struct: in Video_Capabilities_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Flags := C_Struct.Flags;
        Ada_Struct.Min_Bitstream_Buffer_Offset_Alignment :=
            C_Struct.Min_Bitstream_Buffer_Offset_Alignment;
        Ada_Struct.Min_Bitstream_Buffer_Size_Alignment :=
            C_Struct.Min_Bitstream_Buffer_Size_Alignment;
        Ada_Struct.Picture_Access_Granularity :=
            C_Struct.Picture_Access_Granularity;
        Ada_Struct.Min_Coded_Extent := C_Struct.Min_Coded_Extent;
        Ada_Struct.Max_Coded_Extent := C_Struct.Max_Coded_Extent;
        Ada_Struct.Max_DPB_Slots := C_Struct.Max_DPB_Slots;
        Ada_Struct.Max_Active_Reference_Pictures :=
            C_Struct.Max_Active_Reference_Pictures;
        Ada_Struct.Std_Header_Version.Name :=
            Ada.Strings.Unbounded.To_Unbounded_String
                (Interfaces.C.To_Ada
                    (C_Struct.Std_Header_Version.Extension_Name));
        Ada_Struct.Std_Header_Version.Spec_Version :=
            C_Struct.Std_Header_Version.Spec_Version;
    end To_Ada;

    function To_C(Struct: in Extensions.KHR.Physical_Device_Video_Format_Info)
        return Physical_Device_Video_Format_Info_C is
        PDVDIC: Physical_Device_Video_Format_Info_C;
    begin
        PDVDIC.Next := Extension_Records.To_C(Struct.Next);
        PDVDIC.Image_Usage := Struct.Image_Usage;

        return PDVDIC;
    end To_C;

    procedure Free(Struct: in out Physical_Device_Video_Format_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    procedure To_Ada(Ada_Struct: in out Extensions.KHR.Video_Format_Properties;
                     C_Struct: in Video_Format_Properties_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Format := C_Struct.Format;
        Ada_Struct.Component_Mapping := C_Struct.Component_Mapping;
        Ada_Struct.Image_Create_Flags := C_Struct.Image_Create_Flags;
        Ada_Struct.Image_Type := C_Struct.Image_Type;
        Ada_Struct.Image_Tiling := C_Struct.Image_Tiling;
        Ada_Struct.Image_Usage_Flags := C_Struct.Image_Usage_Flags;
    end To_Ada;

    function To_C(Struct: in Extensions.KHR.Video_Picture_Resource_Info)
        return Video_Picture_Resource_Info_C is
        VPRIC: Video_Picture_Resource_Info_C;
    begin
        VPRIC.Next := Extension_Records.To_C(Struct.Next);
        VPRIC.Coded_Offset := Struct.Coded_Offset;
        VPRIC.Coded_Extent := Struct.Coded_Extent;
        VPRIC.Base_Array_Layer := Struct.Base_Array_Layer;
        VPRIC.Image_View_Binding := Struct.Image_View_Binding;

        return VPRIC;
    end To_C;

    procedure Free(Struct: in out Video_Picture_Resource_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Extensions.KHR.Video_Reference_Slot_Info)
        return Video_Reference_Slot_Info_C is
        use type Extensions.KHR.Video_Picture_Resource_Info_Access;

        VRSIC: Video_Reference_Slot_Info_C;
    begin
        VRSIC.Next := Extension_Records.To_C(Struct.Next);
        VRSIC.Slot_Index := Struct.Slot_Index;

        if Struct.Picture_Resource /= null then
            VRSIC.Picture_Resource := new Video_Picture_Resource_Info_C'
                (To_C(Struct.Picture_Resource.all));
        end if;

        return VRSIC;
    end To_C;

    procedure Free(Struct: in out Video_Reference_Slot_Info_C) is
        procedure Free is new Ada.Unchecked_Deallocation
            (Video_Picture_Resource_Info_C,
             Video_Picture_Resource_Info_C_Access);
    begin
        Extension_Records.Free(Struct.Next);

        if Struct.Picture_Resource /= null then
            Free(Struct.Picture_Resource.all);
            Free(Struct.Picture_Resource);
        end if;
    end Free;

    procedure To_Ada
        (Ada_Struct: in out Extensions.KHR.Video_Session_Memory_Requirements;
         C_Struct: in Video_Session_Memory_Requirements_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Memory_Bind_Index := C_Struct.Memory_Bind_Index;
        Ada_Struct.Memory_Requirements := C_Struct.Memory_Requirements;
    end To_Ada;

    function To_C(Struct: in Extensions.KHR.Bind_Video_Session_Memory_Info)
        return Bind_Video_Session_Memory_Info_C is
        BVSMIC: Bind_Video_Session_Memory_Info_C;
    begin
        BVSMIC.Next := Extension_Records.To_C(Struct.Next);
        BVSMIC.Memory_Bind_Index := Struct.Memory_Bind_Index;
        BVSMIC.Memory := Struct.Memory;
        BVSMIC.Memory_Offset := Struct.Memory_Offset;
        BVSMIC.Memory_Size := Struct.Memory_Size;

        return BVSMIC;
    end To_C;

    procedure Free(Struct: in out Bind_Video_Session_Memory_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Extensions.KHR.Video_Session_Create_Info)
        return Video_Session_Create_Info_C is
        use type Extensions.KHR.Video_Profile_Info_Access;

        VSCIC: Video_Session_Create_Info_C;
    begin
        VSCIC.Next := Extension_Records.To_C(Struct.Next);
        VSCIC.Queue_Family_Index := Struct.Queue_Family_Index;
        VSCIC.Flags := Struct.Flags;
        
        if Struct.Video_Profile /= null then
            VSCIC.Video_Profile :=
                new Video_Profile_Info_C'(To_C(Struct.Video_Profile.all));
        end if;

        VSCIC.Picture_Format := Struct.Picture_Format;
        VSCIC.Max_Coded_Extent := Struct.Max_Coded_Extent;
        VSCIC.Reference_Picture_Format := Struct.Reference_Picture_Format;
        VSCIC.Max_DPB_Slots := Struct.Max_DPB_Slots;
        VSCIC.Max_Active_Reference_Pictures :=
            Struct.Max_Active_Reference_Pictures;

        if Struct.Std_Header_Version /= null then
            VSCIC.Std_Header_Version := new C.Extension_Properties_C;
            VSCIC.Std_Header_Version.Extension_Name :=
                Interfaces.C.To_C
                    (Ada.Strings.Unbounded.To_String
                        (Struct.Std_Header_Version.Name));
            VSCIC.Std_Header_Version.Spec_Version :=
                Struct.Std_Header_Version.Spec_Version;
        end if;

        return VSCIC;
    end To_C;

    procedure Free(Struct: in out Video_Session_Create_Info_C) is
        procedure Free is new Ada.Unchecked_Deallocation
            (Video_Profile_Info_C, Video_Profile_Info_C_Access);
        procedure Free is new Ada.Unchecked_Deallocation
            (C.Extension_Properties_C, C.Extension_Properties_C_Access);
    begin
        Extension_Records.Free(Struct.Next);

        if Struct.Video_Profile /= null then
            Free(Struct.Video_Profile.all);
            Free(Struct.Video_Profile);
        end if;

        Free(Struct.Std_Header_Version);
    end Free;

    function To_C
        (Struct: in Extensions.KHR.Video_Session_Parameters_Create_Info)
        return Video_Session_Parameters_Create_Info_C is
        VSPCIC: Video_Session_Parameters_Create_Info_C;
    begin
        VSPCIC.Next := Extension_Records.To_C(Struct.Next);
        VSPCIC.Flags := Struct.Flags;
        VSPCIC.Video_Session_Parameters_Template :=
            Struct.Video_Session_Parameters_Template;
        VSPCIC.Video_Session := Struct.Video_Session;

        return VSPCIC;
    end To_C;

    procedure Free(Struct: in out Video_Session_Parameters_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C
        (Struct: in Extensions.KHR.Video_Session_Parameters_Update_Info)
        return Video_Session_Parameters_Update_Info_C is
        VSPUIC: Video_Session_Parameters_Update_Info_C;
    begin
        VSPUIC.Next := Extension_Records.To_C(Struct.Next);
        VSPUIC.Update_Sequence_Count := Struct.Update_Sequence_Count;

        return VSPUIC;
    end To_C;

    procedure Free(Struct: in out Video_Session_Parameters_Update_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Extensions.KHR.Video_Begin_Coding_Info)
        return Video_Begin_Coding_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array_Convert
            (Video_Reference_Slot_Info_C_Arrays,
             Extensions.KHR.Video_Reference_Slot_Info_Vectors);

        VBCIC: Video_Begin_Coding_Info_C;
    begin
        VBCIC.Next := Extension_Records.To_C(Struct.Next);
        VBCIC.Flags := Struct.Flags;
        VBCIC.Video_Session := Struct.Video_Session;
        VBCIC.Video_Session_Parameters := Struct.Video_Session_Parameters;
        To_C_Array(VBCIC.Reference_Slot_Count,
                   Struct.Reference_Slots,
                   VBCIC.Reference_Slots);

        return VBCIC;
    end To_C;

    procedure Free(Struct: in out Video_Begin_Coding_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        Video_Reference_Slot_Info_C_Arrays.Free(Struct.Reference_Slots,
                                                Free'Access);
    end Free;

    function To_C(Struct: in Extensions.KHR.Video_End_Coding_Info)
        return Video_End_Coding_Info_C is
        VECIC: Video_End_Coding_Info_C;
    begin
        VECIC.Next := Extension_Records.To_C(Struct.Next);
        VECIC.Flags := Struct.Flags;

        return VECIC;
    end To_C;

    procedure Free(Struct: in out Video_End_Coding_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Extensions.KHR.Video_Coding_Control_Info)
        return Video_Coding_Control_Info_C is
        VCCIC: Video_Coding_Control_Info_C;
    begin
        VCCIC.Next := Extension_Records.To_C(Struct.Next);
        VCCIC.Flags := Struct.Flags;

        return VCCIC;
    end To_C;

    procedure Free(Struct: in out Video_Coding_Control_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    procedure To_Ada
        (Ada_Struct: in out Extensions.KHR.Video_Decode_Capabilities;
         C_Struct: in Video_Decode_Capabilities_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Flags := C_Struct.Flags;
    end To_Ada;

    function To_C(Struct: in Extensions.KHR.Video_Decode_Usage_Info)
        return Video_Decode_Usage_Info_C is
        VDUIC: Video_Decode_Usage_Info_C;
    begin
        VDUIC.Next := Extension_Records.To_C(Struct.Next);
        VDUIC.Video_Usage_Hints := Struct.Video_Usage_Hints;

        return VDUIC;
    end To_C;

    procedure Free(Struct: in out Video_Decode_Usage_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Extensions.KHR.Video_Decode_Info)
        return Video_Decode_Info_C is
        use type Extensions.KHR.Video_Reference_Slot_Info_Access;

        procedure To_C_Array is new Utilities.To_C_Array_Convert
            (Video_Reference_Slot_Info_C_Arrays,
             Extensions.KHR.Video_Reference_Slot_Info_Vectors,
             To_C);

        VDIC: Video_Decode_Info_C;
    begin
        VDIC.Next := Extension_Records.To_C(Struct.Next);
        VDIC.Flags := Struct.Flags;
        VDIC.Src_Buffer := Struct.Src_Buffer;
        VDIC.Src_Buffer_Offset := Struct.Src_Buffer_Offset;
        VDIC.Src_Buffer_Range := Struct.Src_Buffer_Range;
        VDIC.Dst_Picture_Resource := To_C(Struct.Dst_Picture_Resource);
        
        if Struct.Setup_Reference_Slot /= null then
            VDIC.Setup_Reference_Slot :=
                new Video_Reference_Slot_Info_C'
                    (To_C(Struct.Setup_Reference_Slot.all));
        end if;

        To_C_Array(VDIC.Reference_Slot_Count,
                   Struct.Reference_Slots,
                   VDIC.Reference_Slots);

        return VDIC;
    end To_C;

    procedure Free(Struct: in out Video_Decode_Info_C) is
        procedure Free is new Ada.Unchecked_Deallocation
            (Video_Reference_Slot_Info_C,
             Video_Reference_Slot_Info_C_Access);
    begin
        Extension_Records.Free(Struct.Next);

        if Struct.Setup_Reference_Slot /= null then
            Free(Struct.Setup_Reference_Slot.all);
            Free(Struct.Setup_Reference_Slot);
        end if;

        Video_Reference_Slot_Info_C_Arrays.Free
            (Struct.Reference_Slots, Free'Access);
    end Free;

    procedure To_Ada
        (Ada_Struct: in out Extensions.KHR.Video_Encode_H264_Capabilities;
         C_Struct: in Video_Encode_H264_Capabilities_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Flags := C_Struct.Flags;
        Ada_Struct.Max_Level_IDC := C_Struct.Max_Level_IDC;
        Ada_Struct.Max_Slice_Count := C_Struct.Max_Slice_Count;
        Ada_Struct.Max_PPicture_L0_Reference_Count :=
            C_Struct.Max_PPicture_L0_Reference_Count;
        Ada_Struct.Max_BPicture_L0_Reference_Count :=
            C_Struct.Max_BPicture_L0_Reference_Count;
        Ada_Struct.Max_L1_Reference_Count := C_Struct.Max_L1_Reference_Count;
        Ada_Struct.Max_Temporal_Layer_Count :=
            C_Struct.Max_Temporal_Layer_Count;
        Ada_Struct.Expect_Dyadic_Temporal_Layer_Pattern :=
            Utilities.To_Ada(C_Struct.Expect_Dyadic_Temporal_Layer_Pattern);
        Ada_Struct.Min_QP := C_Struct.Min_QP;
        Ada_Struct.Max_QP := C_Struct.Max_QP;
        Ada_Struct.Prefers_GOP_Remaining_Frames :=
            Utilities.To_Ada(C_Struct.Prefers_GOP_Remaining_Frames);
        Ada_Struct.Requires_GOP_Remaining_Frames :=
            Utilities.To_Ada(C_Struct.Requires_GOP_Remaining_Frames);
        Ada_Struct.Std_Syntax_Flags := C_Struct.Std_Syntax_Flags;
    end To_Ada;

    procedure To_Ada
        (Ada_Struct:
            in out Extensions.KHR.Video_Encode_H264_Quality_Level_Properties;
         C_Struct: in Video_Encode_H264_Quality_Level_Properties_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Preferred_Rate_Control_Flags :=
            C_Struct.Preferred_Rate_Control_Flags;
        Ada_Struct.Preferred_GOP_Frame_Count :=
            C_Struct.Preferred_GOP_Frame_Count;
        Ada_Struct.Preferred_IDR_Period := C_Struct.Preferred_IDR_Period;
        Ada_Struct.Preferred_Consecutive_B_Frame_Count :=
            C_Struct.Preferred_Consecutive_B_Frame_Count;
        Ada_Struct.Preferred_Temporal_Layer_Count :=
            C_Struct.Preferred_Temporal_Layer_Count;
        Ada_Struct.Preferred_Constant_QP := C_Struct.Preferred_Constant_QP;
        Ada_Struct.Preferred_Max_L0_Reference_Count :=
            C_Struct.Preferred_Max_L0_Reference_Count;
        Ada_Struct.Preferred_Max_L1_Reference_Count :=
            C_Struct.Preferred_Max_L1_Reference_Count;
        Ada_Struct.Preferred_Std_Entropy_Coding_Mode_Flag :=
            Utilities.To_Ada(C_Struct.Preferred_Std_Entropy_Coding_Mode_Flag);
    end To_Ada;

    function To_C
        (Struct: in Extensions.KHR.Video_Encode_H264_Session_Create_Info)
        return Video_Encode_H264_Session_Create_Info_C is
        VEHSCIC: Video_Encode_H264_Session_Create_Info_C;
    begin
        VEHSCIC.Next := Extension_Records.To_C(Struct.Next);
        VEHSCIC.Use_Max_Level_IDC := Utilities.To_C(Struct.Use_Max_Level_IDC);
        VEHSCIC.Max_Level_IDC := Struct.Max_Level_IDC;

        return VEHSCIC;
    end To_C;

    procedure Free(Struct: in out Video_Encode_H264_Session_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C
        (Struct:
            in Extensions.KHR.Video_Encode_H264_Session_Parameters_Add_Info)
        return Video_Encode_H264_Session_Parameters_Add_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array
            (H264_Sequence_Parameter_Set_Arrays,
             Extensions.Std_Video.H264.Sequence_Parameter_Set_Vectors);

        procedure To_C_Array is new Utilities.To_C_Array
            (H264_Picture_Parameter_Set_Arrays,
             Extensions.Std_Video.H264.Picture_Parameter_Set_Vectors);

        VEHSPAIC: Video_Encode_H264_Session_Parameters_Add_Info_C;
    begin
        VEHSPAIC.Next := Extension_Records.To_C(Struct.Next);
        To_C_Array(VEHSPAIC.Sts_SPS_Count, Struct.Std_SPSs, VEHSPAIC.Std_SPSs);
        To_C_Array(VEHSPAIC.Std_PPS_Count, Struct.Std_PPSs, VEHSPAIC.Std_PPSs);

        return VEHSPAIC;
    end To_C;

    procedure Free
        (Struct: in out Video_Encode_H264_Session_Parameters_Add_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        H264_Sequence_Parameter_Set_Arrays.Free(Struct.Std_SPSs);
        H264_Picture_Parameter_Set_Arrays.Free(Struct.Std_PPSs);
    end Free;

    function To_C
        (Struct:
            in Extensions.KHR.Video_Encode_H264_Session_Parameters_Create_Info)
        return Video_Encode_H264_Session_Parameters_Create_Info_C is
        use type
            Extensions.KHR.Video_Encode_H264_Session_Parameters_Add_Info_Access;
        VEHSPCIC: Video_Encode_H264_Session_Parameters_Create_Info_C;
    begin
        VEHSPCIC.Next := Extension_Records.To_C(Struct.Next);
        VEHSPCIC.Max_Std_SPS_Count := Struct.Max_Std_SPS_Count;
        VEHSPCIC.Max_Std_PPS_Count := Struct.Max_Std_PPS_Count;

        if Struct.Parameters_Add_Info /= null then
            VEHSPCIC.Parameters_Add_Info :=
                new Video_Encode_H264_Session_Parameters_Add_Info_C'
                    (To_C(Struct.Parameters_Add_Info.all));
        end if;

        return VEHSPCIC;
    end To_C;

    procedure Free
        (Struct: in out Video_Encode_H264_Session_Parameters_Create_Info_C) is
        procedure Free is
            new Ada.Unchecked_Deallocation
                (Video_Encode_H264_Session_Parameters_Add_Info_C,
                 Video_Encode_H264_Session_Parameters_Add_Info_C_Access);
    begin
        Extension_Records.Free(Struct.Next);

        if Struct.Parameters_Add_Info /= null then
            Free(Struct.Parameters_Add_Info.all);
            Free(Struct.Parameters_Add_Info);
        end if;
    end Free;

    function To_C
        (Struct:
            in Extensions.KHR.Video_Encode_H264_Session_Parameters_Get_Info)
        return Video_Encode_H264_Session_Parameters_Get_Info_C is
        VEHSPGIC: Video_Encode_H264_Session_Parameters_Get_Info_C;
    begin
        VEHSPGIC.Next := Extension_Records.To_C(Struct.Next);
        VEHSPGIC.Write_Std_SPS := Utilities.To_C(Struct.Write_Std_SPS);
        VEHSPGIC.Write_Std_PPS := Utilities.To_C(Struct.Write_Std_PPS);
        VEHSPGIC.Std_SPS_ID := Struct.Std_SPS_ID;
        VEHSPGIC.Std_PPS_ID := Struct.Std_PPS_ID;

        return VEHSPGIC;
    end To_C;

    procedure Free
        (Struct: in out Video_Encode_H264_Session_Parameters_Get_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    procedure To_Ada
        (Ada_Struct:
            in out
              Extensions.KHR.Video_Encode_H264_Session_Parameters_Feedback_Info;
         C_Struct: in Video_Encode_H264_Session_Parameters_Feedback_Info_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Has_Std_SPS_Overrides :=
            Utilities.To_Ada(C_Struct.Has_Std_SPS_Overrides);
        Ada_Struct.Has_Std_PPS_Overrides :=
            Utilities.To_Ada(C_Struct.Has_Std_PPS_Overrides);
    end To_Ada;

    function To_C(Struct: in Extensions.KHR.Video_Encode_H264_Nalu_Slice_Info)
        return Video_Encode_H264_Nalu_Slice_Info_C is
        VEHNSIC: Video_Encode_H264_Nalu_Slice_Info_C;
    begin
        VEHNSIC.Next := Extension_Records.To_C(Struct.Next);
        VEHNSIC.Constant_QP := Struct.Constant_QP;
        VEHNSIC.Std_Slice_Header := Struct.Std_Slice_Header;

        return VEHNSIC;
    end To_C;

    procedure Free(Struct: in out Video_Encode_H264_Nalu_Slice_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Extensions.KHR.Video_Encode_H264_Picture_Info)
        return Video_Encode_H264_Picture_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array_Convert
            (Video_Encode_H264_Nalu_Slice_Info_C_Arrays,
             Extensions.KHR.Video_Encode_H264_Nalu_Slice_Info_Vectors);

        VEHPIC: Video_Encode_H264_Picture_Info_C;
    begin
        VEHPIC.Next := Extension_Records.To_C(Struct.Next);
        To_C_Array(VEHPIC.Nalu_Slice_Entry_Count,
                   Struct.Nalu_Slice_Entries,
                   VEHPIC.Nalu_Slice_Entries);
        VEHPIC.Generate_Prefix_Nalu :=
            Utilities.To_C(Struct.Generate_Prefix_Nalu);

        return VEHPIC;
    end To_C;

    procedure Free(Struct: in out Video_Encode_H264_Picture_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        Video_Encode_H264_Nalu_Slice_Info_C_Arrays.Free
            (Struct.Nalu_Slice_Entries, Free'Access);
    end Free;

    function To_C(Struct: in Extensions.KHR.Video_Encode_H264_DPB_Slot_Info)
        return Video_Encode_H264_DPB_Slot_Info_C is
        VEHDLIC: Video_Encode_H264_DPB_Slot_Info_C;
    begin
        VEHDLIC.Next := Extension_Records.To_C(Struct.Next);
        VEHDLIC.Std_Reference_Info := Struct.Std_Reference_Info;

        return VEHDLIC;
    end To_C;

    procedure Free(Struct: in out Video_Encode_H264_DPB_Slot_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Extensions.KHR.Video_Encode_H264_Profile_Info)
        return Video_Encode_H264_Profile_Info_C is
        VEHPIC: Video_Encode_H264_Profile_Info_C;
    begin
        VEHPIC.Next := Extension_Records.To_C(Struct.Next);
        VEHPIC.Std_Profile_IDC := Struct.Std_Profile_IDC;

        return VEHPIC;
    end To_C;

    procedure Free(Struct: in out Video_Encode_H264_Profile_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Extensions.KHR.Video_Encode_H264_Rate_Control_Info)
        return Video_Encode_H264_Rate_Control_Info_C is
        VEHRCIC: Video_Encode_H264_Rate_Control_Info_C;
    begin
        VEHRCIC.Next := Extension_Records.To_C(Struct.Next);
        VEHRCIC.Flags := Struct.Flags;
        VEHRCIC.GOP_Frame_Count := Struct.GOP_Frame_Count;
        VEHRCIC.IDR_Period := Struct.IDR_Period;
        VEHRCIC.Consecutive_B_Frame_Count := Struct.Consecutive_B_Frame_Count;
        VEHRCIC.Temporal_Layer_Count := Struct.Temporal_Layer_Count;

        return VEHRCIC;
    end To_C;

    procedure Free(Struct: in out Video_Encode_H264_Rate_Control_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C
        (Struct: in Extensions.KHR.Video_Encode_H264_Rate_Control_Layer_Info)
        return Video_Encode_H264_Rate_Control_Layer_Info_C is
        VEHRCLIC: Video_Encode_H264_Rate_Control_Layer_Info_C;
    begin
        VEHRCLIC.Next := Extension_Records.To_C(Struct.Next);
        VEHRCLIC.Use_Min_QP := Utilities.To_C(Struct.Use_Min_QP);
        VEHRCLIC.Min_QP := Struct.Min_QP;
        VEHRCLIC.Use_Max_QP := Utilities.To_C(Struct.Use_Max_QP);
        VEHRCLIC.Max_QP := Struct.Max_QP;
        VEHRCLIC.Use_Max_Frame_Size :=
            Utilities.To_C(Struct.Use_Max_Frame_Size);
        VEHRCLIC.Max_Frame_Size := Struct.Max_Frame_Size;

        return VEHRCLIC;
    end To_C;

    procedure Free
        (Struct: in out Video_Encode_H264_Rate_Control_Layer_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C
        (Struct: in Extensions.KHR.Video_Encode_H264_GOP_Remaining_Frame_Info)
        return Video_Encode_H264_GOP_Remaining_Frame_Info_C is
        VEHGRFIC: Video_Encode_H264_GOP_Remaining_Frame_Info_C;
    begin
        VEHGRFIC.Next := Extension_Records.To_C(Struct.Next);
        VEHGRFIC.Use_GOP_Remaining_Frames :=
            Utilities.To_C(Struct.Use_GOP_Remaining_Frames);
        VEHGRFIC.GOP_Remaining_I := Struct.GOP_Remaining_I;
        VEHGRFIC.GOP_Remaining_P := Struct.GOP_Remaining_P;
        VEHGRFIC.GOP_Remaining_B := Struct.GOP_Remaining_B;

        return VEHGRFIC;
    end To_C;

    procedure Free
        (Struct: in out Video_Encode_H264_GOP_Remaining_Frame_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    procedure To_Ada
        (Ada_Struct: in out Extensions.KHR.Video_Encode_H265_Capabilities;
         C_Struct: in Video_Encode_H265_Capabilities_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Flags := C_Struct.Flags;
        Ada_Struct.Max_Level_IDC := C_Struct.Max_Level_IDC;
        Ada_Struct.Max_Slice_Segment_Count := C_Struct.Max_Slice_Segment_Count;
        Ada_Struct.Max_Tiles := C_Struct.Max_Tiles;
        Ada_Struct.CTB_Sizes := C_Struct.CTB_Sizes;
        Ada_Struct.Transform_Block_Sizes := C_Struct.Transform_Block_Sizes;
        Ada_Struct.Max_P_Picture_L0_Reference_Count :=
            C_Struct.Max_P_Picture_L0_Reference_Count;
        Ada_Struct.Max_B_Picture_L0_Reference_Count :=
            C_Struct.Max_B_Picture_L0_Reference_Count;
        Ada_Struct.Max_L1_Reference_Count := C_Struct.Max_L1_Reference_Count;
        Ada_Struct.Max_Sub_Layer_Count := C_Struct.Max_Sub_Layer_Count;
        Ada_Struct.Expect_Dyadic_Temporal_Sub_Layer_Pattern :=
            Utilities.To_Ada(C_Struct.Expect_Dyadic_Temporal_Sub_Layer_Pattern);
        Ada_Struct.Min_QP := C_Struct.Min_QP;
        Ada_Struct.Max_QP := C_Struct.Max_QP;
        Ada_Struct.Prefers_GOP_Remaining_Frames :=
            Utilities.To_Ada(C_Struct.Prefers_GOP_Remaining_Frames);
        Ada_Struct.Requires_GOP_Remaining_Frames :=
            Utilities.To_Ada(C_Struct.Requires_GOP_Remaining_Frames);
        Ada_Struct.Std_Syntax_Flags := C_Struct.Std_Syntax_Flags;
    end To_Ada;

    function To_C
        (Struct: in Extensions.KHR.Video_Encode_H265_Session_Create_Info)
        return Video_Encode_H265_Session_Create_Info_C is
        VEHSCIC: Video_Encode_H265_Session_Create_Info_C;
    begin
        VEHSCIC.Next := Extension_Records.To_C(Struct.Next);
        VEHSCIC.Use_Max_Level_IDC := Utilities.To_C(Struct.Use_Max_Level_IDC);
        VEHSCIC.Max_Level_IDC := Struct.Max_Level_IDC;

        return VEHSCIC;
    end To_C;

    procedure Free(Struct: in out Video_Encode_H265_Session_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    procedure To_Ada
        (Ada_Struct:
            in out Extensions.KHR.Video_Encode_H265_Quality_Level_Properties;
         C_Struct: in Video_Encode_H265_Quality_Level_Properties_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Preferred_Rate_Control_Flags :=
            C_Struct.Preferred_Rate_Control_Flags;
        Ada_Struct.Preferred_GOP_Frame_Count :=
            C_Struct.Preferred_GOP_Frame_Count;
        Ada_Struct.Preferred_IDR_Period := C_Struct.Preferred_IDR_Period;
        Ada_Struct.Preferred_Consecutive_B_Frame_Count :=
            C_Struct.Preferred_Consecutive_B_Frame_Count;
        Ada_Struct.Preferred_Sub_Layer_Count :=
            C_Struct.Preferred_Sub_Layer_Count;
        Ada_Struct.Preferred_Constant_QP := C_Struct.Preferred_Constant_QP;
        Ada_Struct.Preferred_Max_L0_Reference_Count :=
            C_Struct.Preferred_Max_L0_Reference_Count;
        Ada_Struct.Preferred_Max_L1_Reference_Count :=
            C_Struct.Preferred_Max_L1_Reference_Count;
    end To_Ada;

    function To_C
        (Struct:
            in Extensions.KHR.Video_Encode_H265_Session_Parameters_Add_Info)
        return Video_Encode_H265_Session_Parameters_Add_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array
            (H265_Video_Parameter_Set_Arrays,
             Extensions.Std_Video.H265.Video_Parameter_Set_Vectors);
        procedure To_C_Array is new Utilities.To_C_Array
            (H265_Sequence_Parameter_Set_Arrays,
             Extensions.Std_Video.H265.Sequence_Parameter_Set_Vectors);
        procedure To_C_Array is new Utilities.To_C_Array
            (H265_Picture_Parameter_Set_Arrays,
             Extensions.Std_Video.H265.Picture_Parameter_Set_Vectors);

        VEHSPAIC: Video_Encode_H265_Session_Parameters_Add_Info_C;
    begin
        VEHSPAIC.Next := Extension_Records.To_C(Struct.Next);
        To_C_Array(VEHSPAIC.Std_VPS_Count, Struct.Std_VPSs, VEHSPAIC.Std_VPSs);
        To_C_Array(VEHSPAIC.Std_SPS_Count, Struct.Std_SPSs, VEHSPAIC.Std_SPSs);
        To_C_Array(VEHSPAIC.Std_PPS_Count, Struct.Std_PPSs, VEHSPAIC.Std_PPSs);

        return VEHSPAIC;
    end To_C;

    procedure Free
        (Struct: in out Video_Encode_H265_Session_Parameters_Add_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        H265_Video_Parameter_Set_Arrays.Free(Struct.Std_VPSs);
        H265_Sequence_Parameter_Set_Arrays.Free(Struct.Std_SPSs);
        H265_Picture_Parameter_Set_Arrays.Free(Struct.Std_PPSs);
    end Free;

    function To_C
        (Struct: 
            in Extensions.KHR.Video_Encode_H265_Session_Parameters_Create_Info)
        return Video_Encode_H265_Session_Parameters_Create_Info_C is
        use type
            Extensions.KHR.Video_Encode_H265_Session_Parameters_Add_Info_Access;

        VEHSPCIC: Video_Encode_H265_Session_Parameters_Create_Info_C;
    begin
        VEHSPCIC.Next := Extension_Records.To_C(Struct.Next);
        VEHSPCIC.Max_Std_VPS_Count := Struct.Max_Std_VPS_Count;
        VEHSPCIC.Max_Std_SPS_Count := Struct.Max_Std_SPS_Count;
        VEHSPCIC.Max_Std_PPS_Count := Struct.Max_Std_PPS_Count;

        if Struct.Parameters_Add_Info /= null then
            VEHSPCIC.Parameters_Add_Info :=
                new Video_Encode_H265_Session_Parameters_Add_Info_C'
                    (To_C(Struct.Parameters_Add_Info.all));
        end if;

        return VEHSPCIC;
    end To_C;

    procedure Free
        (Struct: in out Video_Encode_H265_Session_Parameters_Create_Info_C) is
        procedure Free is new Ada.Unchecked_Deallocation
            (Video_Encode_H265_Session_Parameters_Add_Info_C,
             Video_Encode_H265_Session_Parameters_Add_Info_C_Access);
    begin
        Extension_Records.Free(Struct.Next);

        if Struct.Parameters_Add_Info /= null then
            Free(Struct.Parameters_Add_Info.all);
            Free(Struct.Parameters_Add_Info);
        end if;
    end Free;

    function To_C
        (Struct:
            in Extensions.KHR.Video_Encode_H265_Session_Parameters_Get_Info)
        return Video_Encode_H265_Session_Parameters_Get_Info_C is
        VEHSPGIC: Video_Encode_H265_Session_Parameters_Get_Info_C;
    begin
        VEHSPGIC.Next := Extension_Records.To_C(Struct.Next);
        VEHSPGIC.Write_Std_VPS := Utilities.To_C(Struct.Write_Std_VPS);
        VEHSPGIC.Write_Std_SPS := Utilities.To_C(Struct.Write_Std_SPS);
        VEHSPGIC.Write_Std_PPS := Utilities.To_C(Struct.Write_Std_PPS);
        VEHSPGIC.Std_VPS_ID := Struct.Std_VPS_ID;
        VEHSPGIC.Std_SPS_ID := Struct.Std_SPS_ID;
        VEHSPGIC.Std_PPS_ID := Struct.Std_PPS_ID;

        return VEHSPGIC;
    end To_C;

    procedure Free
        (Struct: in out Video_Encode_H265_Session_Parameters_Get_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    procedure To_Ada
        (Ada_Struct:
            in out
              Extensions.KHR.Video_Encode_H265_Session_Parameters_Feedback_Info;
         C_Struct: in Video_Encode_H265_Session_Parameters_Feedback_Info_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Has_Std_VPS_Overrides :=
            Utilities.To_Ada(C_Struct.Has_Std_VPS_Overrides);
        Ada_Struct.Has_Std_SPS_Overrides :=
            Utilities.To_Ada(C_Struct.Has_Std_SPS_Overrides);
        Ada_Struct.Has_Std_PPS_Overrides :=
            Utilities.To_Ada(C_Struct.Has_Std_PPS_Overrides);
    end To_Ada;

    function To_C
        (Struct: in Extensions.KHR.Video_Encode_H265_Nalu_Slice_Segment_Info)
        return Video_Encode_H265_Nalu_Slice_Segment_Info_C is
        VEHNSSIC: Video_Encode_H265_Nalu_Slice_Segment_Info_C;
    begin
        VEHNSSIC.Next := Extension_Records.To_C(Struct.Next);
        VEHNSSIC.Constant_QP := Struct.Constant_QP;
        VEHNSSIC.Std_Slice_Segment_Header := Struct.Std_Slice_Segment_Header;

        return VEHNSSIC;
    end To_C;

    procedure Free
        (Struct: in out Video_Encode_H265_Nalu_Slice_Segment_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Extensions.KHR.Video_Encode_H265_Picture_Info)
        return Video_Encode_H265_Picture_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array_Convert
            (Video_Encode_H265_Nalu_Slice_Segment_Info_C_Arrays,
             Extensions.KHR.Video_Encode_H265_Nalu_Slice_Segment_Info_Vectors);

        VEHPIC: Video_Encode_H265_Picture_Info_C;
    begin
        VEHPIC.Next := Extension_Records.To_C(Struct.Next);
        To_C_Array(VEHPIC.Nalu_Slice_Segment_Entry_Count,
                   Struct.Nalu_Slice_Segment_Entries,
                   VEHPIC.Nalu_Slice_Segment_Entries);
        VEHPIC.Std_Picture_Info := Struct.Std_Picture_Info;

        return VEHPIC;
    end To_C;

    procedure Free(Struct: in out Video_Encode_H265_Picture_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        Video_Encode_H265_Nalu_Slice_Segment_Info_C_Arrays.Free
            (Struct.Nalu_Slice_Segment_Entries, Free'Access);
    end Free;

    function To_C(Struct: in Extensions.KHR.Video_Decode_H264_Profile_Info)
        return Video_Decode_H264_Profile_Info_C is
        VDHPIC: Video_Decode_H264_Profile_Info_C;
    begin
        VDHPIC.Next := Extension_Records.To_C(Struct.Next);
        VDHPIC.Std_Profile_IDC := Struct.Std_Profile_IDC;
        VDHPIC.Picture_Layout := Struct.Picture_Layout;

        return VDHPIC;
    end To_C;

    procedure Free(Struct: in out Video_Decode_H264_Profile_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    procedure To_Ada
        (Ada_Struct: in out Extensions.KHR.Video_Decode_H264_Capabilities;
         C_Struct: in Video_Decode_H264_Capabilities_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Max_Level_IDC := C_Struct.Max_Level_IDC;
        Ada_Struct.Field_Offset_Granularity :=
            C_Struct.Field_Offset_Granularity;
    end To_Ada;

    function To_C
        (Struct:
            in Extensions.KHR.Video_Decode_H264_Session_Parameters_Add_Info)
        return Video_Decode_H264_Session_Parameters_Add_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array
            (H264_Sequence_Parameter_Set_Arrays,
             Extensions.Std_Video.H264.Sequence_Parameter_Set_Vectors);
        procedure To_C_Array is new Utilities.To_C_Array
            (H264_Picture_Parameter_Set_Arrays,
             Extensions.Std_Video.H264.Picture_Parameter_Set_Vectors);

        VDHSPAIC: Video_Decode_H264_Session_Parameters_Add_Info_C;
    begin
        VDHSPAIC.Next := Extension_Records.To_C(Struct.Next);
        To_C_Array(VDHSPAIC.Std_SPS_Count, Struct.Std_SPSs, VDHSPAIC.Std_SPSs);
        To_C_Array(VDHSPAIC.Std_PPS_Count, Struct.Std_PPSs, VDHSPAIC.Std_PPSs);

        return VDHSPAIC;
    end To_C;

    procedure Free
        (Struct: in out Video_Decode_H264_Session_Parameters_Add_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        H264_Sequence_Parameter_Set_Arrays.Free(Struct.Std_SPSs);
        H264_Picture_Parameter_Set_Arrays.Free(Struct.Std_PPSs);
    end Free;

    function To_C
        (Struct:
            in Extensions.KHR.Video_Decode_H264_Session_Parameters_Create_Info)
        return Video_Decode_H264_Session_Parameters_Create_Info_C is
        use type
            Extensions.KHR.Video_Decode_H264_Session_Parameters_Add_Info_Access;

        VDHSPCIC: Video_Decode_H264_Session_Parameters_Create_Info_C;
    begin
        VDHSPCIC.Next := Extension_Records.To_C(Struct.Next);
        VDHSPCIC.Max_Std_SPS_Count := Struct.Max_Std_SPS_Count;
        VDHSPCIC.Max_Std_PPS_Count := Struct.Max_Std_PPS_Count;

        if Struct.Parameters_Add_Info /= null then
            VDHSPCIC.Parameters_Add_Info :=
                new Video_Decode_H264_Session_Parameters_Add_Info_C'
                    (To_C(Struct.Parameters_Add_Info.all));
        end if;

        return VDHSPCIC;
    end To_C;

    procedure Free
        (Struct: in out Video_Decode_H264_Session_Parameters_Create_Info_C) is
        procedure Free is new Ada.Unchecked_Deallocation
            (Video_Decode_H264_Session_Parameters_Add_Info_C,
             Video_Decode_H264_Session_Parameters_Add_Info_C_Access);
    begin
        Extension_Records.Free(Struct.Next);

        if Struct.Parameters_Add_Info /= null then
            Free(Struct.Parameters_Add_Info.all);
            Free(Struct.Parameters_Add_Info);
        end if;
    end Free;

    function To_C(Struct: in Extensions.KHR.Video_Decode_H264_Picture_Info)
        return Video_Decode_H264_Picture_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array(C.Uint32_t_Arrays,
                                                         Unsigned_32_Vectors);

        VDHPIC: Video_Decode_H264_Picture_Info_C;
    begin
        VDHPIC.Next := Extension_Records.To_C(Struct.Next);
        VDHPIC.Std_Picture_Info := Struct.Std_Picture_Info;
        To_C_Array(VDHPIC.Slice_Count,
                   Struct.Slice_Offsets,
                   VDHPIC.Slice_Offsets);

        return VDHPIC;
    end To_C;

    procedure Free(Struct: in out Video_Decode_H264_Picture_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        C.Uint32_t_Arrays.Free(Struct.Slice_Offsets);
    end Free;

    function To_C(Struct: in Extensions.KHR.Video_Decode_H264_DPB_Slot_Info)
        return Video_Decode_H264_DPB_Slot_Info_C is
        VDHDSIC: Video_Decode_H264_DPB_Slot_Info_C;
    begin
        VDHDSIC.Next := Extension_Records.To_C(Struct.Next);
        VDHDSIC.Std_Reference_Info := Struct.Std_Reference_Info;

        return VDHDSIC;
    end To_C;

    procedure Free(Struct: in out Video_Decode_H264_DPB_Slot_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C
        (Struct:
            in Extensions.KHR.Rendering_Fragment_Shading_Rate_Attachment_Info)
        return Rendering_Fragment_Shading_Rate_Attachment_Info_C is
        RFSRAIC: Rendering_Fragment_Shading_Rate_Attachment_Info_C;
    begin
        RFSRAIC.Next := Extension_Records.To_C(Struct.Next);
        RFSRAIC.Image_View := Struct.Image_View;
        RFSRAIC.Image_Layout := Struct.Image_Layout;
        RFSRAIC.Shading_Rate_Attachment_Texel_Size :=
            Struct.Shading_Rate_Attachment_Texel_Size;

        return RFSRAIC;
    end To_C;

    procedure Free
        (Struct: in out Rendering_Fragment_Shading_Rate_Attachment_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Extensions.KHR.Import_Memory_FD_Info)
        return Import_Memory_FD_Info_C is
        IMDIC: Import_Memory_FD_Info_C;
    begin
        IMDIC.Next := Extension_Records.To_C(Struct.Next);
        IMDIC.Handle_Type := Struct.Handle_Type;
        IMDIC.FD := Struct.FD;

        return IMDIC;
    end To_C;

    procedure Free(Struct: in out Import_Memory_FD_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    procedure To_Ada(Ada_Struct: in out Extensions.KHR.Memory_FD_Properties;
                     C_Struct: in Memory_FD_Properties_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Memory_Type_Bits := C_Struct.Memory_Type_Bits;
    end To_Ada;

    function To_C(Struct: in Extensions.KHR.Memory_Get_FD_Info)
        return Memory_Get_FD_Info_C is
        MGFIC: Memory_Get_FD_Info_C;
    begin
        MGFIC.Next := Extension_Records.To_C(Struct.Next);
        MGFIC.Memory := Struct.Memory;
        MGFIC.Handle_Type := Struct.Handle_Type;
        
        return MGFIC;
    end To_C;

    procedure Free(Struct: in out Memory_Get_FD_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Extensions.KHR.Import_Semaphore_FD_Info)
        return Import_Semaphore_FD_Info_C is
        ISFIC: Import_Semaphore_FD_Info_C;
    begin
        ISFIC.Next := Extension_Records.To_C(Struct.Next);
        ISFIC.Semaphore := Struct.Semaphore;
        ISFIC.Flags := Struct.Flags;
        ISFIC.Handle_Type := Struct.Handle_Type;
        ISFIC.FD := Struct.FD;

        return ISFIC;
    end To_C;

    procedure Free(Struct: in out Import_Semaphore_FD_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Extensions.KHR.Semaphore_Get_FD_Info)
        return Semaphore_Get_FD_Info_C is
        SGFIC: Semaphore_Get_FD_Info_C;
    begin
        SGFIC.Next := Extension_Records.To_C(Struct.Next);
        SGFIC.Semaphore := Struct.Semaphore;
        SGFIC.Handle_Type := Struct.Handle_Type;

        return SGFIC;
    end To_C;

    procedure Free(Struct: in out Semaphore_Get_FD_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Extensions.KHR.Present_Region)
        return Present_Region_C is
        procedure To_C_Array is
            new Utilities.To_C_Array(Rect_Layer_Arrays,
                                     Extensions.KHR.Rect_Layer_Vectors);

        PRC: Present_Region_C;
    begin
        To_C_Array(PRC.Rectangle_Count, Struct.Rectangles, PRC.Rectangles);

        return PRC;
    end To_C;

    procedure Free(Struct: in out Present_Region_C) is
    begin
        Rect_Layer_Arrays.Free(Struct.Rectangles);
    end Free;

    function To_C(Struct: in Extensions.KHR.Present_Regions)
        return Present_Regions_C is
        procedure To_C_Array is
            new Utilities.To_C_Array_Convert
                (Present_Region_C_Arrays,
                 Extensions.KHR.Present_Region_Vectors);

        PRC: Present_Regions_C;
        Dummy: Interfaces.Unsigned_32;
    begin
        PRC.Next := Extension_Records.To_C(Struct.Next);
        PRC.Swapchain_Count := Struct.Swapchain_Count;
        To_C_Array(Dummy, Struct.Regions, PRC.Regions);

        return PRC;
    end To_C;

    procedure Free(Struct: in out Present_Regions_C) is
    begin
        Extension_Records.Free(Struct.Next);
        Present_Region_C_Arrays.Free(Struct.Regions, Free'Access);
    end Free;

    procedure To_Ada
        (Ada_Struct: in out Extensions.KHR.Shared_Present_Surface_Capabilities;
         C_Struct: in Shared_Present_Surface_Capabilities_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Shared_Present_Supported_Usage_Flags :=
            C_Struct.Shared_Present_Supported_Usage_Flags;
    end To_Ada;

    function To_C(Struct: in Extensions.KHR.Import_Fence_FD_Info)
        return Import_Fence_FD_Info_C is
        IFFIC: Import_Fence_FD_Info_C;
    begin
        IFFIC.Next := Extension_Records.To_C(Struct.Next);
        IFFIC.Fence := Struct.Fence;
        IFFIC.Flags := Struct.Flags;
        IFFIC.Handle_Type := Struct.Handle_Type;
        IFFIC.FD := Struct.FD;

        return IFFIC;
    end To_C;

    procedure Free(Struct: in out Import_Fence_FD_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Extensions.KHR.Fence_Get_FD_Info)
        return Fence_Get_FD_Info_C is
        FGFIC: Fence_Get_FD_Info_C;
    begin
        FGFIC.Next := Extension_Records.To_C(Struct.Next);
        FGFIC.Fence := Struct.Fence;
        FGFIC.Handle_Type := Struct.Handle_Type;

        return FGFIC;
    end To_C;

    procedure Free(Struct: in out Fence_Get_FD_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    procedure To_Ada
        (Ada_Struct:
            in out Extensions.KHR.Physical_Device_Performance_Query_Features;
         C_Struct: in Physical_Device_Performance_Query_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Performance_Counter_Query_Pools :=
            Utilities.To_Ada(C_Struct.Performance_Counter_Query_Pools);
        Ada_Struct.Performance_Counter_Multiple_Query_Pools :=
            Utilities.To_Ada(C_Struct.Performance_Counter_Multiple_Query_Pools);
    end To_Ada;

    procedure To_Ada
        (Ada_Struct:
            in out Extensions.KHR.Physical_Device_Performance_Query_Properties;
         C_Struct: in Physical_Device_Performance_Query_Properties_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Allow_Command_Buffer_Query_Copies :=
            Utilities.To_Ada(C_Struct.Allow_Command_Buffer_Query_Copies);
    end To_Ada;

    procedure To_Ada(Ada_Struct: in out Extensions.KHR.Performance_Counter;
                     C_Struct: in Performance_Counter_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Unit := C_Struct.Unit;
        Ada_Struct.Scope := C_Struct.Scope;
        Ada_Struct.Storage := C_Struct.Storage;
        Ada_Struct.UUID := C_Struct.UUID;
    end To_Ada;

    procedure To_Ada
        (Ada_Struct: in out Extensions.KHR.Performance_Counter_Description;
         C_Struct: in Performance_Counter_Description_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Flags := C_Struct.Flags;
        Ada.Strings.Unbounded.Set_Unbounded_String
            (Ada_Struct.Name, Interfaces.C.To_Ada(C_Struct.Name));
        Ada.Strings.Unbounded.Set_Unbounded_String
            (Ada_Struct.Category, Interfaces.C.To_Ada(C_Struct.Category));
        Ada.Strings.Unbounded.Set_Unbounded_String
            (Ada_Struct.Description, Interfaces.C.To_Ada(C_Struct.Description));
    end To_Ada;

    function To_C(Struct: in Extensions.KHR.Query_Pool_Performance_Create_Info)
        return Query_Pool_Performance_Create_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array(C.Uint32_t_Arrays,
                                                         Unsigned_32_Vectors);

        QPPCIC: Query_Pool_Performance_Create_Info_C;
    begin
        QPPCIC.Next := Extension_Records.To_C(Struct.Next);
        QPPCIC.Queue_Family_Index := Struct.Queue_Family_Index;
        To_C_Array(QPPCIC.Counter_Index_Count,
                   Struct.Counter_Indices,
                   QPPCIC.Counter_Indices);

        return QPPCIC;
    end To_C;

    procedure Free(Struct: in out Query_Pool_Performance_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        C.Uint32_t_Arrays.Free(Struct.Counter_Indices);
    end Free;

    function To_C(Struct: in Extensions.KHR.Acquire_Profiling_Lock_Info)
        return Acquire_Profiling_Lock_Info_C is
        APLIC: Acquire_Profiling_Lock_Info_C;
    begin
        APLIC.Next := Extension_Records.To_C(Struct.Next);
        APLIC.Flags := Struct.Flags;
        APLIC.Timeout := Struct.Timeout;

        return APLIC;
    end To_C;

    procedure Free(Struct: in out Acquire_Profiling_Lock_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Extensions.KHR.Performance_Query_Submit_Info)
        return Performance_Query_Submit_Info_C is
        PQSIC: Performance_Query_Submit_Info_C;
    begin
        PQSIC.Next := Extension_Records.To_C(Struct.Next);
        PQSIC.Counter_Pass_Index := Struct.Counter_Pass_Index;

        return PQSIC;
    end To_C;

    procedure Free(Struct: in out Performance_Query_Submit_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Extensions.KHR.Physical_Device_Surface_Info_2)
        return Physical_Device_Surface_Info_2_C is
        PDSI2C: Physical_Device_Surface_Info_2_C;
    begin
        PDSI2C.Next := Extension_Records.To_C(Struct.Next);
        PDSI2C.Surface := Struct.Surface;

        return PDSI2C;
    end To_C;

    procedure Free(Struct: in out Physical_Device_Surface_Info_2_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    procedure To_Ada(Ada_Struct: in out Extensions.KHR.Surface_Capabilities_2;
                     C_Struct: in Surface_Capabilities_2_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Surface_Capabilities := C_Struct.Surface_Capabilities;
    end To_Ada;

    procedure To_Ada(Ada_Struct: in out Extensions.KHR.Surface_Format_2;
                     C_Struct: in Surface_Format_2_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Surface_Format := C_Struct.Surface_Format;
    end To_Ada;

    procedure To_Ada(Ada_Struct: in out Extensions.KHR.Display_Properties_2;
                     C_Struct: in Display_Properties_2_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Display_Properties :=
            C_KHR.To_Ada(C_Struct.Display_Properties);
    end To_Ada;

    procedure To_Ada
        (Ada_Struct: in out Extensions.KHR.Display_Plane_Properties_2;
         C_Struct: in Display_Plane_Properties_2_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Display_Plane_Properties :=
            C_Struct.Display_Plane_Properties;
    end To_Ada;

    procedure To_Ada
        (Ada_Struct: in out Extensions.KHR.Display_Mode_Properties_2;
         C_Struct: in Display_Mode_Properties_2_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Display_Mode_Properties := C_Struct.Display_Mode_Properties;
    end To_Ada;

    procedure To_Ada
        (Ada_Struct: in out Extensions.KHR.Display_Plane_Capabilities_2;
         C_Struct: in Display_Plane_Capabilities_2_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Capabilities := C_Struct.Capabilities;
    end To_Ada;

    function To_C(Struct: in Extensions.KHR.Display_Plane_Info_2)
        return Display_Plane_Info_2_C is
        DPI2C: Display_Plane_Info_2_C;
    begin
        DPI2C.Next := Extension_Records.To_C(Struct.Next);
        DPI2C.Mode := Struct.Mode;
        DPI2C.Plane_Index := Struct.Plane_Index;

        return DPI2C;
    end To_C;

    procedure Free(Struct: in out Display_Plane_Info_2_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    procedure To_Ada
        (Ada_Struct:
            in out Extensions.KHR.Physical_Device_Shader_Clock_Features;
         C_Struct: in Physical_Device_Shader_Clock_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Shader_Subgroup_Clock :=
            Utilities.To_Ada(C_Struct.Shader_Subgroup_Clock);
        Ada_Struct.Shader_Device_Clock :=
            Utilities.To_Ada(C_Struct.Shader_Device_Clock);
    end To_Ada;

    function To_C(Struct: in Extensions.KHR.Video_Decode_H265_Profile_Info)
        return Video_Decode_H265_Profile_Info_C is
        VDHPIC: Video_Decode_H265_Profile_Info_C;
    begin
        VDHPIC.Next := Extension_Records.To_C(Struct.Next);
        VDHPIC.Std_Profile_IDC := Struct.Std_Profile_IDC;

        return VDHPIC;
    end To_C;
    
    procedure To_Ada
        (Ada_Struct: in out Extensions.KHR.Video_Decode_H265_Capabilities;
         C_Struct: in Video_Decode_H265_Capabilities_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Max_Level_IDC := C_Struct.Max_Level_IDC;
    end To_Ada;

    procedure Free(Struct: in out Video_Decode_H265_Profile_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C
        (Struct:
            in Extensions.KHR.Video_Decode_H265_Session_Parameters_Add_Info)
        return Video_Decode_H265_Session_Parameters_Add_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array
            (H265_Video_Parameter_Set_Arrays,
             Extensions.Std_Video.H265.Video_Parameter_Set_Vectors);
        procedure To_C_Array is new Utilities.To_C_Array
            (H265_Sequence_Parameter_Set_Arrays,
             Extensions.Std_Video.H265.Sequence_Parameter_Set_Vectors);
        procedure To_C_Array is new Utilities.To_C_Array
            (H265_Picture_Parameter_Set_Arrays,
             Extensions.Std_Video.H265.Picture_Parameter_Set_Vectors);

        VDHSPAIC: Video_Decode_H265_Session_Parameters_Add_Info_C;
    begin
        VDHSPAIC.Next := Extension_Records.To_C(Struct.Next);
        To_C_Array(VDHSPAIC.Std_VPS_Count, Struct.Std_VPSs, VDHSPAIC.Std_VPSs);
        To_C_Array(VDHSPAIC.Std_SPS_Count, Struct.Std_SPSs, VDHSPAIC.Std_SPSs);
        To_C_Array(VDHSPAIC.Std_PPS_Count, Struct.Std_PPSs, VDHSPAIC.Std_PPSs);

        return VDHSPAIC;
    end To_C;

    procedure Free
        (Struct: in out Video_Decode_H265_Session_Parameters_Add_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        H265_Video_Parameter_Set_Arrays.Free(Struct.Std_VPSs);
        H265_Sequence_Parameter_Set_Arrays.Free(Struct.Std_SPSs);
        H265_Picture_Parameter_Set_Arrays.Free(Struct.Std_PPSs);
    end Free;

    function To_C
        (Struct:
            in Extensions.KHR.Video_Decode_H265_Session_Parameters_Create_Info)
        return Video_Decode_H265_Session_Parameters_Create_Info_C is
        use type
            Extensions.KHR.Video_Decode_H265_Session_Parameters_Add_Info_Access;
        VDHSPCIC: Video_Decode_H265_Session_Parameters_Create_Info_C;
    begin
        VDHSPCIC.Next := Extension_Records.To_C(Struct.Next);
        VDHSPCIC.Max_Std_VPS_Count := Struct.Max_Std_VPS_Count;
        VDHSPCIC.Max_Std_SPS_Count := Struct.Max_Std_SPS_Count;
        VDHSPCIC.Max_Std_PPS_Count := Struct.Max_Std_PPS_Count;

        if Struct.Parameters_Add_Info /= null then
            VDHSPCIC.Parameters_Add_Info :=
                new Video_Decode_H265_Session_Parameters_Add_Info_C'
                    (To_C(Struct.Parameters_Add_Info.all));
        end if;

        return VDHSPCIC;
    end To_C;

    procedure Free
        (Struct: in out Video_Decode_H265_Session_Parameters_Create_Info_C) is
        procedure Free is new Ada.Unchecked_Deallocation
            (Video_Decode_H265_Session_Parameters_Add_Info_C,
             Video_Decode_H265_Session_Parameters_Add_Info_C_Access);
    begin
        Extension_Records.Free(Struct.Next);

        if Struct.Parameters_Add_Info /= null then
            Free(Struct.Parameters_Add_Info.all);
            Free(Struct.Parameters_Add_Info);
        end if;
    end Free;

    function To_C(Struct: in Extensions.KHR.Video_Decode_H265_Picture_Info)
        return Video_Decode_H265_Picture_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array(C.Uint32_t_Arrays,
                                                         Unsigned_32_Vectors);

        VDHPIC: Video_Decode_H265_Picture_Info_C;
    begin
        VDHPIC.Next := Extension_Records.To_C(Struct.Next);
        VDHPIC.Std_Picture_Info := Struct.Std_Picture_Info;
        To_C_Array(VDHPIC.Slice_Segment_Count,
                   Struct.Slice_Segment_Offsets,
                   VDHPIC.Slice_Segment_Offsets);

        return VDHPIC;
    end To_C;

    procedure Free(Struct: in out Video_Decode_H265_Picture_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        C.Uint32_t_Arrays.Free(Struct.Slice_Segment_Offsets);
    end Free;

    function To_C(Struct: in Extensions.KHR.Video_Encode_H265_DPB_Slot_Info)
        return Video_Encode_H265_DPB_Slot_Info_C is
        VEHDSIC: Video_Encode_H265_DPB_Slot_Info_C;
    begin
        VEHDSIC.Next := Extension_Records.To_C(Struct.Next);
        VEHDSIC.Std_Reference_Info := Struct.Std_Reference_Info;

        return VEHDSIC;
    end To_C;

    procedure Free(Struct: in out Video_Encode_H265_DPB_Slot_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Extensions.KHR.Video_Encode_H265_Profile_Info)
        return Video_Encode_H265_Profile_Info_C is
        VEHPIC: Video_Encode_H265_Profile_Info_C;
    begin
        VEHPIC.Next := Extension_Records.To_C(Struct.Next);
        VEHPIC.Std_Profile_IDC := Struct.Std_Profile_IDC;

        return VEHPIC;
    end To_C;

    procedure Free(Struct: in out Video_Encode_H265_Profile_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Extensions.KHR.Video_Encode_H265_Rate_Control_Info)
        return Video_Encode_H265_Rate_Control_Info_C is
        VEHRCIC: Video_Encode_H265_Rate_Control_Info_C;
    begin
        VEHRCIC.Next := Extension_Records.To_C(Struct.Next);
        VEHRCIC.Flags := Struct.Flags;
        VEHRCIC.GOP_Frame_Count := Struct.GOP_Frame_Count;
        VEHRCIC.IDR_Period := Struct.IDR_Period;
        VEHRCIC.Consecutive_B_Frame_Count := Struct.Consecutive_B_Frame_Count;
        VEHRCIC.Sub_Layer_Count := Struct.Sub_Layer_Count;

        return VEHRCIC;
    end To_C;

    procedure Free(Struct: in out Video_Encode_H265_Rate_Control_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C
        (Struct: in Extensions.KHR.Video_Encode_H265_Rate_Control_Layer_Info)
        return Video_Encode_H265_Rate_Control_Layer_Info_C is
        VEHRCLIC: Video_Encode_H265_Rate_Control_Layer_Info_C;
    begin
        VEHRCLIC.Next := Extension_Records.To_C(Struct.Next);
        VEHRCLIC.Use_Min_QP := Utilities.To_C(Struct.Use_Min_QP);
        VEHRCLIC.Min_QP := Struct.Min_QP;
        VEHRCLIC.Use_Max_QP := Utilities.To_C(Struct.Use_Max_QP);
        VEHRCLIC.Max_QP := Struct.Max_QP;
        VEHRCLIC.Use_Max_Frame_Size :=
            Utilities.To_C(Struct.Use_Max_Frame_Size);
        VEHRCLIC.Max_Frame_Size := Struct.Max_Frame_Size;

        return VEHRCLIC;
    end To_C;

    procedure Free
        (Struct: in out Video_Encode_H265_Rate_Control_Layer_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C
        (Struct: in Extensions.KHR.Video_Encode_H265_GOP_Remaining_Frame_Info)
        return Video_Encode_H265_GOP_Remaining_Frame_Info_C is
        VEHGRFIC: Video_Encode_H265_GOP_Remaining_Frame_Info_C;
    begin
        VEHGRFIC.Next := Extension_Records.To_C(Struct.Next);
        VEHGRFIC.Use_GOP_Remaining_Frames :=
            Utilities.To_C(Struct.Use_GOP_Remaining_Frames);
        VEHGRFIC.GOP_Remaining_I := Struct.GOP_Remaining_I;
        VEHGRFIC.GOP_Remaining_P := Struct.GOP_Remaining_P;
        VEHGRFIC.GOP_Remaining_B := Struct.GOP_Remaining_B;

        return VEHGRFIC;
    end To_C;

    procedure Free
        (Struct: in out Video_Encode_H265_GOP_Remaining_Frame_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Extensions.KHR.Video_Decode_H265_DPB_Slot_Info)
        return Video_Decode_H265_DPB_Slot_Info_C is
        VDHDSIC: Video_Decode_H265_DPB_Slot_Info_C;
    begin
        VDHDSIC.Next := Extension_Records.To_C(Struct.Next);
        VDHDSIC.Std_Reference_Info := Struct.Std_Reference_Info;

        return VDHDSIC;
    end To_C;

    procedure Free(Struct: in out Video_Decode_H265_DPB_Slot_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C
        (Struct: in Extensions.KHR.Fragment_Shading_Rate_Attachment_Info)
        return Fragment_Shading_Rate_Attachment_Info_C is
        FSRAIC: Fragment_Shading_Rate_Attachment_Info_C;
    begin
        FSRAIC.Next := Extension_Records.To_C(Struct.Next);

        if Struct.Fragment_Shading_Rate_Attachment /= null then
            FSRAIC.Fragment_Shading_Rate_Attachment :=
                new C_V1_2.Attachment_Reference_2_C'
                    (C_V1_2.To_C(Struct.Fragment_Shading_Rate_Attachment.all));
        end if;

        FSRAIC.Shading_Rate_Attachment_Texel_Size :=
            Struct.Shading_Rate_Attachment_Texel_Size;

        return FSRAIC;
    end To_C;

    procedure Free(Struct: in out Fragment_Shading_Rate_Attachment_Info_C) is
        use type C_V1_2.Attachment_Reference_2_C_Access;

        procedure Free is new Ada.Unchecked_Deallocation
            (C_V1_2.Attachment_Reference_2_C,
             C_V1_2.Attachment_Reference_2_C_Access);
    begin
        Extension_Records.Free(Struct.Next);

        if Struct.Fragment_Shading_Rate_Attachment /= null then
            C_V1_2.Free(Struct.Fragment_Shading_Rate_Attachment.all);
            Free(Struct.Fragment_Shading_Rate_Attachment);
        end if;
    end Free;

    function To_C
        (Struct:
            in Extensions.KHR.Pipeline_Fragment_Shading_Rate_State_Create_Info)
        return Pipeline_Fragment_Shading_Rate_State_Create_Info_C is
        PFSRSCIC: Pipeline_Fragment_Shading_Rate_State_Create_Info_C;
    begin
        PFSRSCIC.Next := Extension_Records.To_C(Struct.Next);
        PFSRSCIC.Fragment_Size := Struct.Fragment_Size;
        PFSRSCIC.Combiner_Ops := Struct.Combiner_Ops;

        return PFSRSCIC;
    end To_C;

    procedure Free
        (Struct: in out Pipeline_Fragment_Shading_Rate_State_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    procedure To_Ada
        (Ada_Struct:
            in out
                Extensions.KHR.Physical_Device_Fragment_Shading_Rate_Features;
         C_Struct: in Physical_Device_Fragment_Shading_Rate_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Pipeline_Fragment_Shading_Rate :=
            Utilities.To_Ada(C_Struct.Pipeline_Fragment_Shading_Rate);
        Ada_Struct.Primitive_Fragment_Shading_Rate :=
            Utilities.To_Ada(C_Struct.Primitive_Fragment_Shading_Rate);
        Ada_Struct.Attachment_Fragment_Shading_Rate :=
            Utilities.To_Ada(C_Struct.Attachment_Fragment_Shading_Rate);
    end To_Ada;

    procedure To_Ada
        (Ada_Struct:
            in out
                Extensions.KHR.Physical_Device_Fragment_Shading_Rate_Properties;
         C_Struct: in Physical_Device_Fragment_Shading_Rate_Properties_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Min_Fragment_Shading_Rate_Attachment_Texel_Size :=
            C_Struct.Min_Fragment_Shading_Rate_Attachment_Texel_Size;
        Ada_Struct.Max_Fragment_Shading_Rate_Attachment_Texel_Size :=
            C_Struct.Max_Fragment_Shading_Rate_Attachment_Texel_Size;
        Ada_Struct.
            Max_Fragment_Shading_Rate_Attachment_Texel_Size_Aspect_Ratio :=
            C_Struct.
                Max_Fragment_Shading_Rate_Attachment_Texel_Size_Aspect_Ratio;
        Ada_Struct.Primitive_Fragment_Shading_Rate_With_Multiple_Viewports :=
            Utilities.To_Ada
                (C_Struct.
                    Primitive_Fragment_Shading_Rate_With_Multiple_Viewports);
        Ada_Struct.Layered_Shading_Rate_Attachments :=
            Utilities.To_Ada(C_Struct.Layered_Shading_Rate_Attachments);
        Ada_Struct.Fragment_Shading_Rate_Non_Trivial_Combiner_Ops :=
            Utilities.To_Ada
                (C_Struct.Fragment_Shading_Rate_Non_Trivial_Combiner_Ops);
        Ada_Struct.Max_Fragment_Size := C_Struct.Max_Fragment_Size;
        Ada_Struct.Max_Fragment_Size_Aspect_Ratio :=
            C_Struct.Max_Fragment_Size_Aspect_Ratio;
        Ada_Struct.Max_Fragment_Shading_Rate_Coverage_Samples :=
            C_Struct.Max_Fragment_Shading_Rate_Coverage_Samples;
        Ada_Struct.Max_Fragment_Shading_Rate_Rasterization_Samples :=
            C_Struct.Max_Fragment_Shading_Rate_Rasterization_Samples;
        Ada_Struct.Fragment_Shading_Rate_With_Shader_Depth_Stencil_Writes :=
            Utilities.To_Ada
                (C_Struct.
                    Fragment_Shading_Rate_With_Shader_Depth_Stencil_Writes);
        Ada_Struct.Fragment_Shading_Rate_With_Sample_Mask :=
            Utilities.To_Ada(C_Struct.Fragment_Shading_Rate_With_Sample_Mask);
        Ada_Struct.Fragment_Shading_Rate_With_Shader_Sample_Mask :=
            Utilities.To_Ada
                (C_Struct.Fragment_Shading_Rate_With_Shader_Sample_Mask);
        Ada_Struct.Fragment_Shading_Rate_With_Conservative_Rasterization :=
            Utilities.To_Ada
                (C_Struct.
                    Fragment_Shading_Rate_With_Conservative_Rasterization);
        Ada_Struct.Fragment_Shading_Rate_With_Fragment_Shader_Interlock :=
            Utilities.To_Ada
                (C_Struct.Fragment_Shading_Rate_With_Fragment_Shader_Interlock);
        Ada_Struct.Fragment_Shading_Rate_With_Custom_Sample_Locations :=
            Utilities.To_Ada
                (C_Struct.Fragment_Shading_Rate_With_Custom_Sample_Locations);
        Ada_Struct.Fragment_Shading_Rate_Strict_Multiply_Combiner := 
            Utilities.To_Ada
                (C_Struct.Fragment_Shading_Rate_Strict_Multiply_Combiner);
    end To_Ada;

    procedure To_Ada
        (Ada_Struct:
            in out Extensions.KHR.Physical_Device_Fragment_Shading_Rate;
         C_Struct: in Physical_Device_Fragment_Shading_Rate_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Sample_Counts := C_Struct.Sample_Counts;
        ADa_Struct.Fragment_Size := C_Struct.Fragment_Size;
    end To_Ada;
                 
    procedure To_Ada
        (Ada_Struct:
            in out Extensions.KHR.Physical_Device_Shader_Quad_Control_Features;
         C_Struct: in Physical_Device_Shader_Quad_Control_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Shader_Quad_Control :=
            Utilities.To_Ada(C_Struct.Shader_Quad_Control);
    end To_Ada;

    function To_C(Struct: in Extensions.KHR.Surface_Protected_Capabilities)
        return Surface_Protected_Capabilities_C is
        SPCC: Surface_Protected_Capabilities_C;
    begin
        SPCC.Next := Extension_Records.To_C(Struct.Next);
        SPCC.Supports_Protected := Utilities.To_C(Struct.Supports_Protected);

        return SPCC;
    end To_C;

    procedure Free(Struct: in out Surface_Protected_Capabilities_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    procedure To_Ada
        (Ada_Struct:
            in out Extensions.KHR.Physical_Device_Present_Wait_Features;
         C_Struct: in Physical_Device_Present_Wait_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Present_Wait := Utilities.To_Ada(C_Struct.Present_Wait);
    end To_Ada;

    procedure To_Ada
        (Ada_Struct: in out
         Extensions.KHR.Physical_Device_Pipeline_Executable_Properties_Features;
         C_Struct: in
            Physical_Device_Pipeline_Executable_Properties_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Pipeline_Executable_Info :=
            Utilities.To_Ada(C_Struct.Pipeline_Executable_Info);
    end To_Ada;

    function To_C(Struct: in Extensions.KHR.Pipeline_Info)
        return Pipeline_Info_C is
        PIC: Pipeline_Info_C;
    begin
        PIC.Next := Extension_Records.To_C(Struct.Next);
        PIC.Pipeline := Struct.Pipeline;

        return PIC;
    end To_C;

    procedure Free(Struct: in out Pipeline_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    procedure To_Ada
        (Ada_Struct: in out Extensions.KHR.Pipeline_Executable_Properties;
         C_Struct: in Pipeline_Executable_Properties_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Stages := C_Struct.Stages;
        Ada.Strings.Unbounded.Set_Unbounded_String
            (Ada_Struct.Name, Interfaces.C.To_Ada(C_Struct.Name));
        Ada.Strings.Unbounded.Set_Unbounded_String
            (Ada_Struct.Description, Interfaces.C.To_Ada(C_Struct.Description));
        Ada_Struct.Subgroup_Size := C_Struct.Subgroup_Size;
    end To_Ada;

    function To_C(Struct: in Extensions.KHR.Pipeline_Executable_Info)
        return Pipeline_Executable_Info_C is
        PEIC: Pipeline_Executable_Info_C;
    begin
        PEIC.Next := Extension_Records.To_C(Struct.Next);
        PEIC.Pipeline := Struct.Pipeline;
        PEIC.Executable_Index := Struct.Executable_Index;

        return PEIC;
    end To_C;

    procedure Free(Struct: in out Pipeline_Executable_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    procedure To_Ada
        (Ada_Struct: in out Extensions.KHR.Pipeline_Executable_Statistic;
         C_Struct: in Pipeline_Executable_Statistic_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada.Strings.Unbounded.Set_Unbounded_String
            (Ada_Struct.Name, Interfaces.C.To_Ada(C_Struct.Name));
        Ada.Strings.Unbounded.Set_Unbounded_String
            (Ada_Struct.Description, Interfaces.C.To_Ada(C_Struct.Description));
        
        case Ada_Struct.Format is
            when Extensions.KHR.Bool32 =>
                Ada_Struct.Value.B32 := Utilities.To_Ada(C_Struct.B32);
            when Extensions.KHR.Int64 =>
                Ada_Struct.Value.I64 := C_Struct.I64;
            when Extensions.KHR.Uint64 =>
                Ada_Struct.Value.U64 := C_Struct.U64;
            when Extensions.KHR.Float64 =>
                Ada_Struct.Value.F64 := C_Struct.F64;
        end case;
    end To_Ada;

    procedure To_Ada
        (Ada_Struct:
            in out Extensions.KHR.Pipeline_Executable_Internal_Representation;
         C_Struct: in Pipeline_Executable_Internal_Representation_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada.Strings.Unbounded.Set_Unbounded_String
            (Ada_Struct.Name, Interfaces.C.To_Ada(C_Struct.Name));
        Ada.Strings.Unbounded.Set_Unbounded_String
            (Ada_Struct.Description, Interfaces.C.To_Ada(C_Struct.Description));
        Ada_Struct.Is_Text := Utilities.To_Ada(C_Struct.Is_Text);
        Ada_Struct.Data_Size := C_Struct.Data_Size;
        Ada_Struct.Data := C_Struct.Data;
    end To_Ada;

    function To_C(Struct: in Extensions.KHR.Pipeline_Library_Create_Info)
        return Pipeline_Library_Create_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array(Pipeline_Arrays,
                                                         Pipeline_Vectors);

        PLCIC: Pipeline_Library_Create_Info_C;
    begin
        PLCIC.Next := Extension_Records.To_C(Struct.Next);
        To_C_Array(PLCIC.Library_Count, Struct.Libraries, PLCIC.Libraries);

        return PLCIC;
    end To_C;

    procedure Free(Struct: in out Pipeline_Library_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        Pipeline_Arrays.Free(Struct.Libraries);
    end Free;

    function To_C(Struct: in Extensions.KHR.Present_ID) return Present_ID_C is
        procedure To_C_Array is
            new Utilities.To_C_Array(Uint64_t_Arrays,
                                     Extensions.KHR.Unsigned_64_Vectors);

        PIC: Present_ID_C;
    begin
        PIC.Next := Extension_Records.To_C(Struct.Next);
        To_C_Array(PIC.Swapchain_Count, Struct.Present_IDs, PIC.Present_IDs);

        return PIC;
    end To_C;

    procedure Free(Struct: in out Present_ID_C) is
    begin
        Extension_Records.Free(Struct.Next);
        Uint64_t_Arrays.Free(Struct.Present_IDs);
    end Free;

    procedure To_Ada
        (Ada_Struct: in out Extensions.KHR.Physical_Device_Present_ID_Features;
         C_Struct: in Physical_Device_Present_ID_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Present_ID := Utilities.To_Ada(C_Struct.Present_ID);
    end To_Ada;

    function To_C(Struct: in Extensions.KHR.Video_Encode_Info)
        return Video_Encode_Info_C is
        use type Extensions.KHR.Video_Reference_Slot_Info_Access;

        procedure To_C_Array is new Utilities.To_C_Array_Convert
            (Video_Reference_Slot_Info_C_Arrays,
             Extensions.KHR.Video_Reference_Slot_Info_Vectors);

        VEIC: Video_Encode_Info_C;
    begin
        VEIC.Next := Extension_Records.To_C(Struct.Next);
        VEIC.Flags := Struct.Flags;
        VEIC.Dst_Buffer := Struct.Dst_Buffer;
        VEIC.Dst_Buffer_Offset := Struct.Dst_Buffer_Offset;
        VEIC.Dst_Buffer_Range := Struct.Dst_Buffer_Range;
        VEIC.Src_Picture_Resource := To_C(Struct.Src_Picture_Resource);

        if Struct.Setup_Reference_Slot /= null then
            VEIC.Setup_Reference_Slot := new Video_Reference_Slot_Info_C'
                (To_C(Struct.Setup_Reference_Slot.all));
        end if;

        To_C_Array(VEIC.Reference_Slot_Count,
                   Struct.Reference_Slots,
                   VEIC.Reference_Slots);
        VEIC.Preceding_Externally_Encoded_Bytes :=
            Struct.Preceding_Externally_Encoded_Bytes;

        return VEIC;
    end To_C;

    procedure Free(Struct: in out Video_Encode_Info_C) is
        procedure Free is
            new Ada.Unchecked_Deallocation(Video_Reference_Slot_Info_C,
                                           Video_Reference_Slot_Info_C_Access);
    begin
        Extension_Records.Free(Struct.Next);
        Free(Struct.Src_Picture_Resource);

        if Struct.Setup_Reference_Slot /= null then
            Free(Struct.Setup_Reference_Slot.all);
            Free(Struct.Setup_Reference_Slot);
        end if;

        Video_Reference_Slot_Info_C_Arrays.Free(Struct.Reference_Slots,
                                                Free'Access);
    end Free;

    procedure To_Ada
        (Ada_Struct: in out Extensions.KHR.Video_Encode_Capabilities;
         C_Struct: in Video_Encode_Capabilities_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Flags := C_Struct.Flags;
        Ada_Struct.Rate_Control_Modes := C_Struct.Rate_Control_Modes;
        Ada_Struct.Max_Rate_Control_Layers := C_Struct.Max_Rate_Control_Layers;
        Ada_Struct.Max_Bitrate := C_Struct.Max_Bitrate;
        Ada_Struct.Max_Quality_Levels := C_Struct.Max_Quality_Levels;
        Ada_Struct.Encode_Input_Picture_Granularity :=
            C_Struct.Encode_Input_Picture_Granularity;
        Ada_Struct.Supported_Encode_Feedback_Flags :=
            C_Struct.Supported_Encode_Feedback_Flags;
    end To_Ada;

    function To_C
        (Struct: in Extensions.KHR.Query_Pool_Video_Encode_Feedback_Create_Info)
        return Query_Pool_Video_Encode_Feedback_Create_Info_C is
        QPVEFCIC: Query_Pool_Video_Encode_Feedback_Create_Info_C;
    begin
        QPVEFCIC.Next := Extension_Records.To_C(Struct.Next);
        QPVEFCIC.Encode_Feedback_Flags := Struct.Encode_Feedback_Flags;

        return QPVEFCIC;
    end To_C;

    procedure Free
        (Struct: in out Query_Pool_Video_Encode_Feedback_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Extensions.KHR.Video_Encode_Usage_Info)
        return Video_Encode_Usage_Info_C is
        VEUIC: Video_Encode_Usage_Info_C;
    begin
        VEUIC.Next := Extension_Records.To_C(Struct.Next);
        VEUIC.Video_Usage_Hints := Struct.Video_Usage_Hints;
        VEUIC.Video_Content_Hints := Struct.Video_Content_Hints;
        VEUIC.Tuning_Mode := Struct.Tuning_Mode;

        return VEUIC;
    end To_C;

    procedure Free(Struct: in out Video_Encode_Usage_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C
        (Struct: in Extensions.KHR.Video_Encode_Rate_Control_Layer_Info)
        return Video_Encode_Rate_Control_Layer_Info_C is
        VERCLIC: Video_Encode_Rate_Control_Layer_Info_C;
    begin
        VERCLIC.Next := Extension_Records.To_C(Struct.Next);
        VERCLIC.Average_Bitrate := Struct.Average_Bitrate;
        VERCLIC.Max_Bitrate := Struct.Max_Bitrate;
        VERCLIC.Frame_Rate_Numerator := Struct.Frame_Rate_Numerator;
        VERCLIC.Frame_Rate_Denominator := Struct.Frame_Rate_Denominator;

        return VERCLIC;
    end To_C;

    procedure Free(Struct: in out Video_Encode_Rate_Control_Layer_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Extensions.KHR.Video_Encode_Rate_Control_Info)
        return Video_Encode_Rate_Control_Info_C is
        procedure To_C_Array is
            new Utilities.To_C_Array_Convert
                (Video_Encode_Rate_Control_Layer_Info_C_Arrays,
                 Extensions.KHR.Video_Encode_Rate_Control_Layer_Info_Vectors);

        VERCIC: Video_Encode_Rate_Control_Info_C;
    begin
        VERCIC.Next := Extension_Records.To_C(Struct.Next);
        VERCIC.Flags := Struct.Flags;
        VERCIC.Rate_Control_Mode := Struct.Rate_Control_Mode;
        To_C_Array(VERCIC.Layer_Count, Struct.Layers, VERCIC.Layers);
        VERCIC.Virtual_Buffer_Size_In_Ms := Struct.Virtual_Buffer_Size_In_Ms;
        VERCIC.Initial_Virtual_Buffer_Size_In_Ms :=
            Struct.Initial_Virtual_Buffer_Size_In_Ms;

        return VERCIC;
    end To_C;

    procedure Free(Struct: in out Video_Encode_Rate_Control_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        Video_Encode_Rate_Control_Layer_Info_C_Arrays.Free(Struct.Layers,
                                                           Free'Access);
    end Free;

    function To_C
        (Struct:
            in Extensions.KHR.Physical_Device_Video_Encode_Quality_Level_Info)
        return Physical_Device_Video_Encode_Quality_Level_Info_C is
        use type Extensions.KHR.Video_Profile_Info_Access;

        PDVEQLIC: Physical_Device_Video_Encode_Quality_Level_Info_C;
    begin
        PDVEQLIC.Next := Extension_Records.To_C(Struct.Next);

        if Struct.Video_Profile /= null then
            PDVEQLIC.Video_Profile :=
                new Video_Profile_Info_C'(To_C(Struct.Video_Profile.all));
        end if;

        PDVEQLIC.Quality_Level := Struct.Quality_Level;

        return PDVEQLIC;
    end To_C;

    procedure Free
        (Struct: in out Physical_Device_Video_Encode_Quality_Level_Info_C) is
        procedure Free is
            new Ada.Unchecked_Deallocation(Video_Profile_Info_C,
                                           Video_Profile_Info_C_Access);
    begin
        Extension_Records.Free(Struct.Next);

        if Struct.Video_Profile /= null then
            Free(Struct.Video_Profile.all);
            Free(Struct.Video_Profile);
        end if;
    end Free;

    procedure To_Ada
        (Ada_Struct:
            in out Extensions.KHR.Video_Encode_Quality_Level_Properties;
         C_Struct: in Video_Encode_Quality_Level_Properties_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Preferred_Rate_Control_Mode :=
            C_Struct.Preferred_Rate_Control_Mode;
        Ada_Struct.Preferred_Rate_Control_Layer_Count :=
            C_Struct.Preferred_Rate_Control_Layer_Count;
    end To_Ada;

    function To_C(Struct: in Extensions.KHR.Video_Encode_Quality_Level_Info)
        return Video_Encode_Quality_Level_Info_C is
        VEQLIC: Video_Encode_Quality_Level_Info_C;
    begin
        VEQLIC.Next := Extension_Records.To_C(Struct.Next);
        VEQLIC.Quality_Level := Struct.Quality_Level;

        return VEQLIC;
    end To_C;

    procedure Free(Struct: in out Video_Encode_Quality_Level_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C
        (Struct: in Extensions.KHR.Video_Encode_Session_Parameters_Get_Info)
        return Video_Encode_Session_Parameters_Get_Info_C is
        VESPGIC: Video_Encode_Session_Parameters_Get_Info_C;
    begin
        VESPGIC.Next := Extension_Records.To_C(Struct.Next);
        VESPGIC.Video_Session_Parameters := Struct.Video_Session_Parameters;

        return VESPGIC;
    end To_C;

    procedure Free(Struct: in out Video_Encode_Session_Parameters_Get_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    procedure To_Ada
        (Ada_Struct:
            in out Extensions.KHR.Video_Encode_Session_Parameters_Feedback_Info;
         C_Struct: in Video_Encode_Session_Parameters_Feedback_Info_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Has_Overrides := Utilities.To_Ada(C_Struct.Has_Overrides);
    end To_Ada;

    procedure To_Ada
        (Ada_Struct: in out
            Extensions.KHR.Physical_Device_Fragment_Shader_Barycentric_Features;
         C_Struct: in Physical_Device_Fragment_Shader_Barycentric_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Fragment_Shader_Barycentric :=
            Utilities.To_Ada(C_Struct.Fragment_Shader_Barycentric);
    end To_Ada;

    procedure To_Ada
        (Ada_Struct: in out
          Extensions.KHR.Physical_Device_Fragment_Shader_Barycentric_Properties;
         C_Struct:
            in Physical_Device_Fragment_Shader_Barycentric_Properties_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Tri_Strip_Vertex_Order_Independent_Of_Provoking_Vertex :=
            Utilities.To_Ada
                (C_Struct.
                    Tri_Strip_Vertex_Order_Independent_Of_Provoking_Vertex);
    end To_Ada;

    procedure To_Ada
        (Ada_Struct: in out
   Extensions.KHR.Physical_Device_Shader_Subgroup_Uniform_Control_Flow_Features;
         C_Struct: in
            Physical_Device_Shader_Subgroup_Uniform_Control_Flow_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Shader_Subgroup_Uniform_Control_Flow :=
            Utilities.To_Ada(C_Struct.Shader_Subgroup_Uniform_Control_Flow);
    end To_Ada;

    procedure To_Ada
        (Ada_Struct: in out
       Extensions.KHR.Physical_Device_Workgroup_Memory_Explicit_Layout_Features;
         C_Struct: in
            Physical_Device_Workgroup_Memory_Explicit_Layout_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Workgroup_Memory_Explicit_Layout :=
            Utilities.To_Ada(C_Struct.Workgroup_Memory_Explicit_Layout);
        Ada_Struct.Workgroup_Memory_Explicit_Layout_Scalar_Block_Layout :=
            Utilities.To_Ada
                (C_Struct.Workgroup_Memory_Explicit_Layout_Scalar_Block_Layout);
        Ada_Struct.Workgroup_Memory_Explicit_Layout_8_Bit_Access :=
            Utilities.To_Ada
                (C_Struct.Workgroup_Memory_Explicit_Layout_8_Bit_Access);
        Ada_Struct.Workgroup_Memory_Explicit_Layout_16_Bit_Access :=
            Utilities.To_Ada
                (C_Struct.Workgroup_Memory_Explicit_Layout_16_Bit_Access);
    end To_Ada;

    procedure To_Ada
        (Ada_Struct: in out
            Extensions.KHR.Physical_Device_Ray_Tracing_Maintenance_1_Features;
         C_Struct: in Physical_Device_Ray_Tracing_Maintenance_1_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Ray_Tracing_Maintenance_1 :=
            Utilities.To_Ada(C_Struct.Ray_Tracing_Maintenance_1);
        Ada_Struct.Ray_Tracing_Pipeline_Trace_Rays_Indirect_2 :=
            Utilities.To_Ada
                (C_Struct.Ray_Tracing_Pipeline_Trace_Rays_Indirect_2);
    end To_Ada;

    procedure To_Ada
        (Ada_Struct: in out
           Extensions.KHR.Physical_Device_Shader_Maximal_Reconvergence_Features;
         C_Struct:
            in Physical_Device_Shader_Maximal_Reconvergence_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Shader_Maximal_Reconvergence :=
            Utilities.To_Ada(C_Struct.Shader_Maximal_Reconvergence);
    end To_Ada;
 
    procedure To_Ada
        (Ada_Struct:
            in out
             Extensions.KHR.Physical_Device_Ray_Tracing_Position_Fetch_Features;
         C_Struct: in Physical_Device_Ray_Tracing_Position_Fetch_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Ray_Tracing_Position_Fetch :=
            Utilities.To_Ada(C_Struct.Ray_Tracing_Position_Fetch);
    end To_Ada;

    procedure To_Ada
        (Ada_Struct: in out Extensions.KHR.Cooperative_Matrix_Properties;
         C_Struct: in Cooperative_Matrix_Properties_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.M_Size := C_Struct.M_Size;
        Ada_Struct.N_Size := C_Struct.N_Size;
        Ada_Struct.K_Size := C_Struct.K_Size;
        Ada_Struct.A_Type := C_Struct.A_Type;
        Ada_Struct.B_Type := C_Struct.B_Type;
        Ada_Struct.C_Type := C_Struct.C_Type;
        Ada_Struct.Result_Type := C_Struct.Result_Type;
        Ada_Struct.Saturating_Accumulation :=
            Utilities.To_Ada(C_Struct.Saturating_Accumulation);
        Ada_Struct.Scope := C_Struct.Scope;
    end To_Ada;

    procedure To_Ada
        (Ada_Struct:
            in out Extensions.KHR.Physical_Device_Cooperative_Matrix_Features;
         C_Struct: in Physical_Device_Cooperative_Matrix_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Cooperative_Matrix :=
            Utilities.To_Ada(C_Struct.Cooperative_Matrix);
        Ada_Struct.Cooperative_Matrix_Robust_Buffer_Access :=
            Utilities.To_Ada(C_Struct.Cooperative_Matrix_Robust_Buffer_Access);
    end To_Ada;

    procedure To_Ada
        (Ada_Struct:
            in out Extensions.KHR.Physical_Device_Cooperative_Matrix_Properties;
         C_Struct: in Physical_Device_Cooperative_Matrix_Properties_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Cooperative_Matrix_Supported_Stages :=
            C_Struct.Cooperative_Matrix_Supported_Stages;
    end To_Ada;

    function To_C(Struct: in Extensions.KHR.Video_Decode_AV1_Profile_Info)
        return Video_Decode_AV1_Profile_Info_C is
        VDAPIC: Video_Decode_AV1_Profile_Info_C;
    begin
        VDAPIC.Next := Extension_Records.To_C(Struct.Next);
        VDAPIC.Std_Profile := Struct.Std_Profile;
        VDAPIC.Film_Grain_Support := Utilities.To_C(Struct.Film_Grain_Support);

        return VDAPIC;
    end To_C;

    procedure Free(Struct: in out Video_Decode_AV1_Profile_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    procedure To_Ada
        (Ada_Struct: in out Extensions.KHR.Video_Decode_AV1_Capabilities;
         C_Struct: in Video_Decode_AV1_Capabilities_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Max_Level := C_Struct.Max_Level;
    end To_Ada;

    function To_C
        (Struct:
            in Extensions.KHR.Video_Decode_AV1_Session_Parameters_Create_Info)
        return Video_Decode_AV1_Session_Parameters_Create_Info_C is
        VDASPCIC: Video_Decode_AV1_Session_Parameters_Create_Info_C;
    begin
        VDASPCIC.Next := Extension_Records.To_C(Struct.Next);
        VDASPCIC.Std_Sequence_Header := Struct.Std_Sequence_Header;

        return VDASPCIC;
    end To_C;

    procedure Free
        (Struct: in out Video_Decode_AV1_Session_Parameters_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Extensions.KHR.Video_Decode_AV1_Picture_Info)
        return Video_Decode_AV1_Picture_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array(C.Uint32_t_Arrays,
                                                         Unsigned_32_Vectors);

        VDAPIC: Video_Decode_AV1_Picture_Info_C;
    begin
        VDAPIC.Next := Extension_Records.To_C(Struct.Next);
        VDAPIC.Std_Picture_Info := Struct.Std_Picture_Info;
        VDAPIC.Reference_Name_Slot_Indices :=
            Struct.Reference_Name_Slot_Indices;
        VDAPIC.Frame_Header_Offset := Struct.Frame_Header_Offset;
        To_C_Array(VDAPIC.Tile_Count, Struct.Tile_Offsets, VDAPIC.Tile_Offsets);
        To_C_Array(VDAPIC.Tile_Count, Struct.Tile_Sizes, VDAPIC.Tile_Sizes);

        return VDAPIC;
    end To_C;

    procedure Free(Struct: in out Video_Decode_AV1_Picture_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        C.Uint32_T_Arrays.Free(Struct.Tile_Offsets);
        C.Uint32_T_Arrays.Free(Struct.Tile_Sizes);
    end Free;

    function To_C(Struct: in Extensions.KHR.Video_Decode_AV1_DPB_Slot_Info)
        return Video_Decode_AV1_DPB_Slot_Info_C is
        VDADSIC: Video_Decode_AV1_DPB_Slot_Info_C;
    begin
        VDADSIC.Next := Extension_Records.To_C(Struct.Next);
        VDADSIC.Std_Reference_Info := Struct.Std_Reference_Info;

        return VDADSIC;
    end To_C;

    procedure Free(Struct: in out Video_Decode_AV1_DPB_Slot_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    procedure To_Ada
        (Ada_Struct:
            in out Extensions.KHR.Physical_Device_Video_Maintenance_1_Features;
         C_Struct: in Physical_Device_Video_Maintenance_1_Features_C) is
    begin
        Extension_Records.To_Ada(Ada_Struct.Next, C_Struct.Next);
        Ada_Struct.Video_Maintenance_1 :=
            Utilities.To_Ada(C_Struct.Video_Maintenance_1);
    end To_Ada;

    function To_C(Struct: in Extensions.KHR.Video_Inline_Query_Info)
        return Video_Inline_Query_Info_C is
        VIQIC: Video_Inline_Query_Info_C;
    begin
        VIQIC.Next := Extension_Records.To_C(Struct.Next);
        VIQIC.Query_Pool := Struct.Query_Pool;
        VIQIC.First_Query := Struct.First_Query;
        VIQIC.Query_Count := Struct.Query_Count;

        return VIQIC;
    end To_C;

    procedure Free(Struct: in out Video_Inline_Query_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Extensions.KHR.Calibrated_Timestamp_Info)
        return Calibrated_Timestamp_Info_C is
        CTIC: Calibrated_Timestamp_Info_C;
    begin
        CTIC.Next := Extension_Records.To_C(Struct.Next);
        CTIC.Time_Domain := Struct.Time_Domain;

        return CTIC;
    end To_C;

    procedure Free(Struct: in out Calibrated_Timestamp_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Extensions.KHR.Set_Descriptor_Buffer_Offsets_Info)
        return Set_Descriptor_Buffer_Offsets_Info_C is
        procedure To_C_Array is new Utilities.To_C_Array(C.Uint32_t_Arrays,
                                                         Unsigned_32_Vectors);

        procedure To_C_Array is new Utilities.To_C_Array(Device_Size_Arrays,
                                                         Device_Size_Vectors);

        SDBOIC: Set_Descriptor_Buffer_Offsets_Info_C;
    begin
        SDBOIC.Next := Extension_Records.To_C(Struct.Next);
        SDBOIC.Stage_Flags := Struct.Stage_Flags;
        SDBOIC.Layout := Struct.Layout;
        SDBOIC.First_Set := Struct.First_Set;
        To_C_Array(SDBOIC.Set_Count,
                   Struct.Buffer_Indices,
                   SDBOIC.Buffer_Indices);
        To_C_Array(SDBOIC.Set_Count,
                   Struct.Offsets,
                   SDBOIC.Offsets);

        return SDBOIC;
    end To_C;

    procedure Free(Struct: in out Set_Descriptor_Buffer_Offsets_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
        C.Uint32_t_Arrays.Free(Struct.Buffer_Indices);
        Device_Size_Arrays.Free(Struct.Offsets);
    end Free;

    function To_C
        (Struct:
            in Extensions.KHR.Bind_Descriptor_Buffer_Embedded_Samplers_Info)
        return Bind_Descriptor_Buffer_Embedded_Samplers_Info_C is
        BDBESIC: Bind_Descriptor_Buffer_Embedded_Samplers_Info_C;
    begin
        BDBESIC.Next := Extension_Records.To_C(Struct.Next);
        BDBESIC.Stage_Flags := Struct.Stage_Flags;
        BDBESIC.Layout := Struct.Layout;
        BDBESIC.Set := Struct.Set;

        return BDBESIC;
    end To_C;

    procedure Free
        (Struct: in out Bind_Descriptor_Buffer_Embedded_Samplers_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Extensions.KHR.Xlib_Surface_Create_Info)
        return Xlib_Surface_Create_Info_C is
        XSCIC: Xlib_Surface_Create_Info_C;
    begin
        XSCIC.Next := Extension_Records.To_C(Struct.Next);
        XSCIC.Flags := Struct.Flags;
        XSCIC.Dpy := Struct.Dpy;
        XSCIC.Window := Struct.Window;

        return XSCIC;
    end To_C;

    procedure Free(Struct: in out Xlib_Surface_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Extensions.KHR.Xcb_Surface_Create_Info)
        return Xcb_Surface_Create_Info_C is
        XSCIC: Xcb_Surface_Create_Info_C;
    begin
        XSCIC.Next := Extension_Records.To_C(Struct.Next);
        XSCIC.Flags := Struct.Flags;
        XSCIC.Connection := Struct.Connection;
        XSCIC.Window := Struct.Window;

        return XSCIC;
    end To_C;

    procedure Free(Struct: in out Xcb_Surface_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Extensions.KHR.Wayland_Surface_Create_Info)
        return Wayland_Surface_Create_Info_C is
        WSCIC: Wayland_Surface_Create_Info_C;
    begin
        WSCIC.Next := Extension_Records.To_C(Struct.Next);
        WSCIC.Flags := Struct.Flags;
        WSCIC.Display := Struct.Display;
        WSCIC.Surface := Struct.Surface;

        return WSCIC;
    end To_C;

    procedure Free(Struct: in out Wayland_Surface_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Struct: in Extensions.KHR.Win32_Surface_Create_Info)
        return Win32_Surface_Create_Info_C is
        WSCIC: Win32_Surface_Create_Info_C;
    begin
        WSCIC.Next := Extension_Records.To_C(Struct.Next);
        WSCIC.Flags := Struct.Flags;
        WSCIC.Instance := Struct.Instance;
        WSCIC.Window := Struct.Window;

        return WSCIC;
    end To_C;

    procedure Free(Struct: in out Win32_Surface_Create_Info_C) is
    begin
        Extension_Records.Free(Struct.Next);
    end Free;

    function To_C(Next: in In_Structure_Access)
        return C.In_Structure_C_Access is
    begin
        if Next = null then
            return null;
        end if;

        case In_Structure(Next.Record_Type) is
            when Swapchain_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.KHR.Swapchain_Create_Info,
                         Swapchain_Create_Info_C,
                         Swapchain_Create_Info_C_Access,
                         To_C);
                begin
                    return Make_Struct(Next);
                end;
            when Present_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.KHR.Present_Info,
                         Present_Info_C,
                         Present_Info_C_Access,
                         To_C);
                begin
                    return Make_Struct(Next);
                end;
            when Image_Swapchain_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.KHR.Image_Swapchain_Create_Info,
                         Image_Swapchain_Create_Info_C,
                         Image_Swapchain_Create_Info_C_Access,
                         To_C);
                begin
                    return Make_Struct(Next);
                end;
            when Bind_Image_Memory_Swapchain_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.KHR.Bind_Image_Memory_Swapchain_Info,
                         Bind_Image_Memory_Swapchain_Info_C,
                         Bind_Image_Memory_Swapchain_Info_C_Access,
                         To_C);
                begin
                    return Make_Struct(Next);
                end;
            when Acquire_Next_Image_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.KHR.Acquire_Next_Image_Info,
                         Acquire_Next_Image_Info_C,
                         Acquire_Next_Image_Info_C_Access,
                         To_C);
                begin
                    return Make_Struct(Next);
                end;
             when Device_Group_Present_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.KHR.Device_Group_Present_Info,
                         Device_Group_Present_Info_C,
                         Device_Group_Present_Info_C_Access,
                         To_C);
                begin
                    return Make_Struct(Next);
                end;
            when Device_Group_Swapchain_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.KHR.Device_Group_Swapchain_Create_Info,
                         Device_Group_Swapchain_Create_Info_C,
                         Device_Group_Swapchain_Create_Info_C_Access,
                         To_C);
                begin
                    return Make_Struct(Next);
                end;
            when Display_Mode_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.KHR.Display_Mode_Create_Info,
                         Display_Mode_Create_Info_C,
                         Display_Mode_Create_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Display_Surface_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.KHR.Display_Surface_Create_Info,
                         Display_Surface_Create_Info_C,
                         Display_Surface_Create_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Display_Present_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.KHR.Display_Present_Info,
                         Display_Present_Info_C,
                         Display_Present_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Profile_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.KHR.Video_Profile_Info,
                         Video_Profile_Info_C,
                         Video_Profile_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Profile_List_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.KHR.Video_Profile_List_Info,
                         Video_Profile_List_Info_C,
                         Video_Profile_List_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Video_Format_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.KHR.Physical_Device_Video_Format_Info,
                         Physical_Device_Video_Format_Info_C,
                         Physical_Device_Video_Format_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Picture_Resource_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.KHR.Video_Picture_Resource_Info,
                         Video_Picture_Resource_Info_C,
                         Video_Picture_Resource_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Reference_Slot_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.KHR.Video_Reference_Slot_Info,
                         Video_Reference_Slot_Info_C,
                         Video_Reference_Slot_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Bind_Video_Session_Memory_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.KHR.Bind_Video_Session_Memory_Info,
                         Bind_Video_Session_Memory_Info_C,
                         Bind_Video_Session_Memory_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Session_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.KHR.Video_Session_Create_Info,
                         Video_Session_Create_Info_C,
                         Video_Session_Create_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Session_Parameters_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.KHR.Video_Session_Parameters_Create_Info,
                         Video_Session_Parameters_Create_Info_C,
                         Video_Session_Parameters_Create_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Session_Parameters_Update_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.KHR.Video_Session_Parameters_Update_Info,
                         Video_Session_Parameters_Update_Info_C,
                         Video_Session_Parameters_Update_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Begin_Coding_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.KHR.Video_Begin_Coding_Info,
                         Video_Begin_Coding_Info_C,
                         Video_Begin_Coding_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_End_Coding_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.KHR.Video_End_Coding_Info,
                         Video_End_Coding_Info_C,
                         Video_End_Coding_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Coding_Control_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.KHR.Video_Coding_Control_Info,
                         Video_Coding_Control_Info_C,
                         Video_Coding_Control_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Decode_Usage_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.KHR.Video_Decode_Usage_Info,
                         Video_Decode_Usage_Info_C,
                         Video_Decode_Usage_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Decode_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.KHR.Video_Decode_Info,
                         Video_Decode_Info_C,
                         Video_Decode_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Encode_H264_Session_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.KHR.Video_Encode_H264_Session_Create_Info,
                         Video_Encode_H264_Session_Create_Info_C,
                         Video_Encode_H264_Session_Create_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Encode_H264_Session_Parameters_Add_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                  (Extensions.KHR.Video_Encode_H264_Session_Parameters_Add_Info,
                   Video_Encode_H264_Session_Parameters_Add_Info_C,
                   Video_Encode_H264_Session_Parameters_Add_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Encode_H264_Session_Parameters_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
               (Extensions.KHR.Video_Encode_H264_Session_Parameters_Create_Info,
                Video_Encode_H264_Session_Parameters_Create_Info_C,
                Video_Encode_H264_Session_Parameters_Create_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Encode_H264_Session_Parameters_Get_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                  (Extensions.KHR.Video_Encode_H264_Session_Parameters_Get_Info,
                   Video_Encode_H264_Session_Parameters_Get_Info_C,
                   Video_Encode_H264_Session_Parameters_Get_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Encode_H264_Nalu_Slice_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.KHR.Video_Encode_H264_Nalu_Slice_Info,
                         Video_Encode_H264_Nalu_Slice_Info_C,
                         Video_Encode_H264_Nalu_Slice_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Encode_H264_Picture_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.KHR.Video_Encode_H264_Picture_Info,
                         Video_Encode_H264_Picture_Info_C,
                         Video_Encode_H264_Picture_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Encode_H264_DPB_Slot_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.KHR.Video_Encode_H264_DPB_Slot_Info,
                         Video_Encode_H264_DPB_Slot_Info_C,
                         Video_Encode_H264_DPB_Slot_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Encode_H264_Profile_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.KHR.Video_Encode_H264_Profile_Info,
                         Video_Encode_H264_Profile_Info_C,
                         Video_Encode_H264_Profile_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Encode_H264_Rate_Control_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.KHR.Video_Encode_H264_Rate_Control_Info,
                         Video_Encode_H264_Rate_Control_Info_C,
                         Video_Encode_H264_Rate_Control_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Encode_H264_Rate_Control_Layer_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                      (Extensions.KHR.Video_Encode_H264_Rate_Control_Layer_Info,
                       Video_Encode_H264_Rate_Control_Layer_Info_C,
                       Video_Encode_H264_Rate_Control_Layer_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Encode_H264_GOP_Remaining_Frame_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                     (Extensions.KHR.Video_Encode_H264_GOP_Remaining_Frame_Info,
                      Video_Encode_H264_GOP_Remaining_Frame_Info_C,
                      Video_Encode_H264_GOP_Remaining_Frame_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Encode_H265_Session_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                     (Extensions.KHR.Video_Encode_H265_Session_Create_Info,
                      Video_Encode_H265_Session_Create_Info_C,
                      Video_Encode_H265_Session_Create_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Encode_H265_Session_Parameters_Add_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                  (Extensions.KHR.Video_Encode_H265_Session_Parameters_Add_Info,
                   Video_Encode_H265_Session_Parameters_Add_Info_C,
                   Video_Encode_H265_Session_Parameters_Add_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Encode_H265_Session_Parameters_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
               (Extensions.KHR.Video_Encode_H265_Session_Parameters_Create_Info,
                Video_Encode_H265_Session_Parameters_Create_Info_C,
                Video_Encode_H265_Session_Parameters_Create_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Encode_H265_Session_Parameters_Get_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                  (Extensions.KHR.Video_Encode_H265_Session_Parameters_Get_Info,
                   Video_Encode_H265_Session_Parameters_Get_Info_C,
                   Video_Encode_H265_Session_Parameters_Get_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Encode_H265_Nalu_Slice_Segment_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                      (Extensions.KHR.Video_Encode_H265_Nalu_Slice_Segment_Info,
                       Video_Encode_H265_Nalu_Slice_Segment_Info_C,
                       Video_Encode_H265_Nalu_Slice_Segment_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Encode_H265_Picture_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.KHR.Video_Encode_H265_Picture_Info,
                         Video_Encode_H265_Picture_Info_C,
                         Video_Encode_H265_Picture_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Encode_H265_DPB_Slot_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.KHR.Video_Encode_H265_DPB_Slot_Info,
                         Video_Encode_H265_DPB_Slot_Info_C,
                         Video_Encode_H265_DPB_Slot_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Encode_H265_Profile_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.KHR.Video_Encode_H265_Profile_Info,
                         Video_Encode_H265_Profile_Info_C,
                         Video_Encode_H265_Profile_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Encode_H265_Rate_Control_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.KHR.Video_Encode_H265_Rate_Control_Info,
                         Video_Encode_H265_Rate_Control_Info_C,
                         Video_Encode_H265_Rate_Control_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Encode_H265_Rate_Control_Layer_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                      (Extensions.KHR.Video_Encode_H265_Rate_Control_Layer_Info,
                       Video_Encode_H265_Rate_Control_Layer_Info_C,
                       Video_Encode_H265_Rate_Control_Layer_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Encode_H265_GOP_Remaining_Frame_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                     (Extensions.KHR.Video_Encode_H265_GOP_Remaining_Frame_Info,
                      Video_Encode_H265_GOP_Remaining_Frame_Info_C,
                      Video_Encode_H265_GOP_Remaining_Frame_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Decode_H264_Profile_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.KHR.Video_Decode_H264_Profile_Info,
                         Video_Decode_H264_Profile_Info_C,
                         Video_Decode_H264_Profile_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Decode_H264_Session_Parameters_Add_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                  (Extensions.KHR.Video_Decode_H264_Session_Parameters_Add_Info,
                   Video_Decode_H264_Session_Parameters_Add_Info_C,
                   Video_Decode_H264_Session_Parameters_Add_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Decode_H264_Session_Parameters_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
               (Extensions.KHR.Video_Decode_H264_Session_Parameters_Create_Info,
                Video_Decode_H264_Session_Parameters_Create_Info_C,
                Video_Decode_H264_Session_Parameters_Create_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Decode_H264_Picture_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.KHR.Video_Decode_H264_Picture_Info,
                         Video_Decode_H264_Picture_Info_C,
                         Video_Decode_H264_Picture_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Decode_H264_DPB_Slot_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.KHR.Video_Decode_H264_DPB_Slot_Info,
                         Video_Decode_H264_DPB_Slot_Info_C,
                         Video_Decode_H264_DPB_Slot_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Rendering_Fragment_Shading_Rate_Attachment_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                (Extensions.KHR.Rendering_Fragment_Shading_Rate_Attachment_Info,
                 Rendering_Fragment_Shading_Rate_Attachment_Info_C,
                 Rendering_Fragment_Shading_Rate_Attachment_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Import_Memory_FD_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.KHR.Import_Memory_FD_Info,
                         Import_Memory_FD_Info_C,
                         Import_Memory_FD_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Memory_Get_FD_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.KHR.Memory_Get_FD_Info,
                         Memory_Get_FD_Info_C,
                         Memory_Get_FD_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Import_Semaphore_FD_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.KHR.Import_Semaphore_FD_Info,
                         Import_Semaphore_FD_Info_C,
                         Import_Semaphore_FD_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Semaphore_Get_FD_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.KHR.Semaphore_Get_FD_Info,
                         Semaphore_Get_FD_Info_C,
                         Semaphore_Get_FD_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Present_Regions_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.KHR.Present_Regions,
                         Present_Regions_C,
                         Present_Regions_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Import_Fence_FD_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.KHR.Import_Fence_FD_Info,
                         Import_Fence_FD_Info_C,
                         Import_Fence_FD_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Fence_Get_FD_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.KHR.Fence_Get_FD_Info,
                         Fence_Get_FD_Info_C,
                         Fence_Get_FD_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Query_Pool_Performance_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.KHR.Query_Pool_Performance_Create_Info,
                         Query_Pool_Performance_Create_Info_C,
                         Query_Pool_Performance_Create_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Acquire_Profiling_Lock_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.KHR.Acquire_Profiling_Lock_Info,
                         Acquire_Profiling_Lock_Info_C,
                         Acquire_Profiling_Lock_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Performance_Query_Submit_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.KHR.Performance_Query_Submit_Info,
                         Performance_Query_Submit_Info_C,
                         Performance_Query_Submit_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Surface_Info_2_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.KHR.Physical_Device_Surface_Info_2,
                         Physical_Device_Surface_Info_2_C,
                         Physical_Device_Surface_Info_2_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Display_Plane_Info_2_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.KHR.Display_Plane_Info_2,
                         Display_Plane_Info_2_C,
                         Display_Plane_Info_2_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Decode_H265_Profile_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.KHR.Video_Decode_H265_Profile_Info,
                         Video_Decode_H265_Profile_Info_C,
                         Video_Decode_H265_Profile_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Decode_H265_Session_Parameters_Add_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                  (Extensions.KHR.Video_Decode_H265_Session_Parameters_Add_Info,
                   Video_Decode_H265_Session_Parameters_Add_Info_C,
                   Video_Decode_H265_Session_Parameters_Add_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Decode_H265_Session_Parameters_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
               (Extensions.KHR.Video_Decode_H265_Session_Parameters_Create_Info,
                Video_Decode_H265_Session_Parameters_Create_Info_C,
                Video_Decode_H265_Session_Parameters_Create_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Decode_H265_Picture_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.KHR.Video_Decode_H265_Picture_Info,
                         Video_Decode_H265_Picture_Info_C,
                         Video_Decode_H265_Picture_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Decode_H265_DPB_Slot_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.KHR.Video_Decode_H265_DPB_Slot_Info,
                         Video_Decode_H265_DPB_Slot_Info_C,
                         Video_Decode_H265_DPB_Slot_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Fragment_Shading_Rate_Attachment_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.KHR.Fragment_Shading_Rate_Attachment_Info,
                         Fragment_Shading_Rate_Attachment_Info_C,
                         Fragment_Shading_Rate_Attachment_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Pipeline_Fragment_Shading_Rate_State_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
               (Extensions.KHR.Pipeline_Fragment_Shading_Rate_State_Create_Info,
                Pipeline_Fragment_Shading_Rate_State_Create_Info_C,
                Pipeline_Fragment_Shading_Rate_State_Create_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Surface_Protected_Capabilities_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.KHR.Surface_Protected_Capabilities,
                         Surface_Protected_Capabilities_C,
                         Surface_Protected_Capabilities_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Pipeline_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.KHR.Pipeline_Info,
                         Pipeline_Info_C,
                         Pipeline_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Pipeline_Executable_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.KHR.Pipeline_Executable_Info,
                         Pipeline_Executable_Info_C,
                         Pipeline_Executable_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Pipeline_Library_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.KHR.Pipeline_Library_Create_Info,
                         Pipeline_Library_Create_Info_C,
                         Pipeline_Library_Create_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Present_ID_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.KHR.Present_ID,
                         Present_ID_C,
                         Present_ID_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Encode_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.KHR.Video_Encode_Info,
                         Video_Encode_Info_C,
                         Video_Encode_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Query_Pool_Video_Encode_Feedback_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                   (Extensions.KHR.Query_Pool_Video_Encode_Feedback_Create_Info,
                    Query_Pool_Video_Encode_Feedback_Create_Info_C,
                    Query_Pool_Video_Encode_Feedback_Create_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Encode_Usage_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.KHR.Video_Encode_Usage_Info,
                         Video_Encode_Usage_Info_C,
                         Video_Encode_Usage_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Encode_Rate_Control_Layer_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.KHR.Video_Encode_Rate_Control_Layer_Info,
                         Video_Encode_Rate_Control_Layer_Info_C,
                         Video_Encode_Rate_Control_Layer_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Encode_Rate_Control_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.KHR.Video_Encode_Rate_Control_Info,
                         Video_Encode_Rate_Control_Info_C,
                         Video_Encode_Rate_Control_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Video_Encode_Quality_Level_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                (Extensions.KHR.Physical_Device_Video_Encode_Quality_Level_Info,
                 Physical_Device_Video_Encode_Quality_Level_Info_C,
                 Physical_Device_Video_Encode_Quality_Level_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Encode_Quality_Level_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.KHR.Video_Encode_Quality_Level_Info,
                         Video_Encode_Quality_Level_Info_C,
                         Video_Encode_Quality_Level_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Encode_Session_Parameters_Get_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                       (Extensions.KHR.Video_Encode_Session_Parameters_Get_Info,
                        Video_Encode_Session_Parameters_Get_Info_C,
                        Video_Encode_Session_Parameters_Get_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Decode_AV1_Profile_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.KHR.Video_Decode_AV1_Profile_Info,
                         Video_Decode_AV1_Profile_Info_C,
                         Video_Decode_AV1_Profile_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Decode_AV1_Session_Parameters_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                (Extensions.KHR.Video_Decode_AV1_Session_Parameters_Create_Info,
                 Video_Decode_AV1_Session_Parameters_Create_Info_C,
                 Video_Decode_AV1_Session_Parameters_Create_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Decode_AV1_Picture_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.KHR.Video_Decode_AV1_Picture_Info,
                         Video_Decode_AV1_Picture_Info_C,
                         Video_Decode_AV1_Picture_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Decode_AV1_DPB_Slot_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.KHR.Video_Decode_AV1_DPB_Slot_Info,
                         Video_Decode_AV1_DPB_Slot_Info_C,
                         Video_Decode_AV1_DPB_Slot_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Inline_Query_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.KHR.Video_Inline_Query_Info,
                         Video_Inline_Query_Info_C,
                         Video_Inline_Query_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Calibrated_Timestamp_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.KHR.Calibrated_Timestamp_Info,
                         Calibrated_Timestamp_Info_C,
                         Calibrated_Timestamp_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Set_Descriptor_Buffer_Offsets_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.KHR.Set_Descriptor_Buffer_Offsets_Info,
                         Set_Descriptor_Buffer_Offsets_Info_C,
                         Set_Descriptor_Buffer_Offsets_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Bind_Descriptor_Buffer_Embedded_Samplers_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                  (Extensions.KHR.Bind_Descriptor_Buffer_Embedded_Samplers_Info,
                   Bind_Descriptor_Buffer_Embedded_Samplers_Info_C,
                   Bind_Descriptor_Buffer_Embedded_Samplers_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Xlib_Surface_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.KHR.Xlib_Surface_Create_Info,
                         Xlib_Surface_Create_Info_C,
                         Xlib_Surface_Create_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Xcb_Surface_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.KHR.Xcb_Surface_Create_Info,
                         Xcb_Surface_Create_Info_C,
                         Xcb_Surface_Create_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Wayland_Surface_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.KHR.Wayland_Surface_Create_Info,
                         Wayland_Surface_Create_Info_C,
                         Wayland_Surface_Create_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Win32_Surface_Create_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_In_Struct
                        (Extensions.KHR.Win32_Surface_Create_Info,
                         Win32_Surface_Create_Info_C,
                         Win32_Surface_Create_Info_C_Access);
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
            when Device_Group_Present_Capabilities_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Extensions.KHR.Device_Group_Present_Capabilities,
                         Device_Group_Present_Capabilities_C,
                         Device_Group_Present_Capabilities_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Queue_Family_Query_Result_Status_Properties_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                    (Extensions.KHR.Queue_Family_Query_Result_Status_Properties,
                     Queue_Family_Query_Result_Status_Properties_C,
                     Queue_Family_Query_Result_Status_Properties_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Queue_Family_Video_Properties_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Extensions.KHR.Queue_Family_Video_Properties,
                         Queue_Family_Video_Properties_C,
                         Queue_Family_Video_Properties_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Capabilities_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Extensions.KHR.Video_Capabilities,
                         Video_Capabilities_C,
                         Video_Capabilities_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Format_Properties_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Extensions.KHR.Video_Format_Properties,
                         Video_Format_Properties_C,
                         Video_Format_Properties_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Session_Memory_Requirements_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Extensions.KHR.Video_Session_Memory_Requirements,
                         Video_Session_Memory_Requirements_C,
                         Video_Session_Memory_Requirements_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Decode_Capabilities_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Extensions.KHR.Video_Decode_Capabilities,
                         Video_Decode_Capabilities_C,
                         Video_Decode_Capabilities_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Encode_H264_Capabilities_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Extensions.KHR.Video_Encode_H264_Capabilities,
                         Video_Encode_H264_Capabilities_C,
                         Video_Encode_H264_Capabilities_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Encode_H264_Quality_Level_Properties_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                     (Extensions.KHR.Video_Encode_H264_Quality_Level_Properties,
                      Video_Encode_H264_Quality_Level_Properties_C,
                      Video_Encode_H264_Quality_Level_Properties_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Encode_H264_Session_Parameters_Feedback_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
             (Extensions.KHR.Video_Encode_H264_Session_Parameters_Feedback_Info,
              Video_Encode_H264_Session_Parameters_Feedback_Info_C,
              Video_Encode_H264_Session_Parameters_Feedback_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Encode_H265_Capabilities_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Extensions.KHR.Video_Encode_H265_Capabilities,
                         Video_Encode_H265_Capabilities_C,
                         Video_Encode_H265_Capabilities_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Encode_H265_Quality_Level_Properties_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                     (Extensions.KHR.Video_Encode_H265_Quality_Level_Properties,
                      Video_Encode_H265_Quality_Level_Properties_C,
                      Video_Encode_H265_Quality_Level_Properties_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Encode_H265_Session_Parameters_Feedback_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
             (Extensions.KHR.Video_Encode_H265_Session_Parameters_Feedback_Info,
              Video_Encode_H265_Session_Parameters_Feedback_Info_C,
              Video_Encode_H265_Session_Parameters_Feedback_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Decode_H264_Capabilities_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Extensions.KHR.Video_Decode_H264_Capabilities,
                         Video_Decode_H264_Capabilities_C,
                         Video_Decode_H264_Capabilities_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Memory_FD_Properties_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Extensions.KHR.Memory_FD_Properties,
                         Memory_FD_Properties_C,
                         Memory_FD_Properties_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Shared_Present_Surface_Capabilities_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Extensions.KHR.Shared_Present_Surface_Capabilities,
                         Shared_Present_Surface_Capabilities_C,
                         Shared_Present_Surface_Capabilities_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Performance_Query_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                     (Extensions.KHR.Physical_Device_Performance_Query_Features,
                      Physical_Device_Performance_Query_Features_C,
                      Physical_Device_Performance_Query_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Performance_Query_Properties_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                   (Extensions.KHR.Physical_Device_Performance_Query_Properties,
                    Physical_Device_Performance_Query_Properties_C,
                    Physical_Device_Performance_Query_Properties_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Performance_Counter_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Extensions.KHR.Performance_Counter,
                         Performance_Counter_C,
                         Performance_Counter_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Performance_Counter_Description_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Extensions.KHR.Performance_Counter_Description,
                         Performance_Counter_Description_C,
                         Performance_Counter_Description_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Surface_Capabilities_2_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Extensions.KHR.Surface_Capabilities_2,
                         Surface_Capabilities_2_C,
                         Surface_Capabilities_2_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Surface_Format_2_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Extensions.KHR.Surface_Format_2,
                         Surface_Format_2_C,
                         Surface_Format_2_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Display_Properties_2_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Extensions.KHR.Display_Properties_2,
                         Display_Properties_2_C,
                         Display_Properties_2_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Display_Plane_Properties_2_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Extensions.KHR.Display_Plane_Properties_2,
                         Display_Plane_Properties_2_C,
                         Display_Plane_Properties_2_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Display_Mode_Properties_2_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Extensions.KHR.Display_Mode_Properties_2,
                         Display_Mode_Properties_2_C,
                         Display_Mode_Properties_2_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Display_Plane_Capabilities_2_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Extensions.KHR.Display_Plane_Capabilities_2,
                         Display_Plane_Capabilities_2_C,
                         Display_Plane_Capabilities_2_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Shader_Clock_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Extensions.KHR.Physical_Device_Shader_Clock_Features,
                         Physical_Device_Shader_Clock_Features_C,
                         Physical_Device_Shader_Clock_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Decode_H265_Capabilities_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Extensions.KHR.Video_Decode_H265_Capabilities,
                         Video_Decode_H265_Capabilities_C,
                         Video_Decode_H265_Capabilities_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Fragment_Shading_Rate_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                 (Extensions.KHR.Physical_Device_Fragment_Shading_Rate_Features,
                  Physical_Device_Fragment_Shading_Rate_Features_C,
                  Physical_Device_Fragment_Shading_Rate_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Fragment_Shading_Rate_Properties_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
               (Extensions.KHR.Physical_Device_Fragment_Shading_Rate_Properties,
                Physical_Device_Fragment_Shading_Rate_Properties_C,
                Physical_Device_Fragment_Shading_Rate_Properties_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Fragment_Shading_Rate_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Extensions.KHR.Physical_Device_Fragment_Shading_Rate,
                         Physical_Device_Fragment_Shading_Rate_C,
                         Physical_Device_Fragment_Shading_Rate_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Shader_Quad_Control_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                   (Extensions.KHR.Physical_Device_Shader_Quad_Control_Features,
                    Physical_Device_Shader_Quad_Control_Features_C,
                    Physical_Device_Shader_Quad_Control_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Present_Wait_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Extensions.KHR.Physical_Device_Present_Wait_Features,
                         Physical_Device_Present_Wait_Features_C,
                         Physical_Device_Present_Wait_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Pipeline_Executable_Properties_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
        (Extensions.KHR.Physical_Device_Pipeline_Executable_Properties_Features,
         Physical_Device_Pipeline_Executable_Properties_Features_C,
         Physical_Device_Pipeline_Executable_Properties_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Pipeline_Executable_Properties_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Extensions.KHR.Pipeline_Executable_Properties,
                         Pipeline_Executable_Properties_C,
                         Pipeline_Executable_Properties_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Pipeline_Executable_Statistic_Type =>
                declare
                    function To_Out_Structure_C_Access is
                        new Ada.Unchecked_Conversion
                            (Pipeline_Executable_Statistic_C_Access,
                             C.Out_Structure_C_Access);

                    Ada_Struct: Extensions.KHR.Pipeline_Executable_Statistic
                                renames
                        Extensions.KHR.Pipeline_Executable_Statistic(Next.all);
                    C_Struct: Pipeline_Executable_Statistic_C_Access;
                    C_Out: C.Out_Structure_C_Access;
                begin
                    C_Struct := new Pipeline_Executable_Statistic_C
                                    (Ada_Struct.Format);
                    C_Out := To_Out_Structure_C_Access(C_Struct);
                    C_Out.Next := Extension_Records.To_C(Next.Next);

                    return C_Out;
                end;
            when Pipeline_Executable_Internal_Representation_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                    (Extensions.KHR.Pipeline_Executable_Internal_Representation,
                     Pipeline_Executable_Internal_Representation_C,
                     Pipeline_Executable_Internal_Representation_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Present_ID_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Extensions.KHR.Physical_Device_Present_ID_Features,
                         Physical_Device_Present_ID_Features_C,
                         Physical_Device_Present_ID_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Encode_Capabilities_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Extensions.KHR.Video_Encode_Capabilities,
                         Video_Encode_Capabilities_C,
                         Video_Encode_Capabilities_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Encode_Quality_Level_Properties_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Extensions.KHR.Video_Encode_Quality_Level_Properties,
                         Video_Encode_Quality_Level_Properties_C,
                         Video_Encode_Quality_Level_Properties_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Encode_Session_Parameters_Feedback_Info_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                  (Extensions.KHR.Video_Encode_Session_Parameters_Feedback_Info,
                   Video_Encode_Session_Parameters_Feedback_Info_C,
                   Video_Encode_Session_Parameters_Feedback_Info_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Fragment_Shader_Barycentric_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
           (Extensions.KHR.Physical_Device_Fragment_Shader_Barycentric_Features,
            Physical_Device_Fragment_Shader_Barycentric_Features_C,
            Physical_Device_Fragment_Shader_Barycentric_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Fragment_Shader_Barycentric_Properties_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
         (Extensions.KHR.Physical_Device_Fragment_Shader_Barycentric_Properties,
          Physical_Device_Fragment_Shader_Barycentric_Properties_C,
          Physical_Device_Fragment_Shader_Barycentric_Properties_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when
           Physical_Device_Shader_Subgroup_Uniform_Control_Flow_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
  (Extensions.KHR.Physical_Device_Shader_Subgroup_Uniform_Control_Flow_Features,
   Physical_Device_Shader_Subgroup_Uniform_Control_Flow_Features_C,
   Physical_Device_Shader_Subgroup_Uniform_Control_Flow_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when
               Physical_Device_Workgroup_Memory_Explicit_Layout_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
      (Extensions.KHR.Physical_Device_Workgroup_Memory_Explicit_Layout_Features,
       Physical_Device_Workgroup_Memory_Explicit_Layout_Features_C,
       Physical_Device_Workgroup_Memory_Explicit_Layout_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Ray_Tracing_Maintenance_1_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
             (Extensions.KHR.Physical_Device_Ray_Tracing_Maintenance_1_Features,
              Physical_Device_Ray_Tracing_Maintenance_1_Features_C,
              Physical_Device_Ray_Tracing_Maintenance_1_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Shader_Maximal_Reconvergence_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
          (Extensions.KHR.Physical_Device_Shader_Maximal_Reconvergence_Features,
           Physical_Device_Shader_Maximal_Reconvergence_Features_C,
           Physical_Device_Shader_Maximal_Reconvergence_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Ray_Tracing_Position_Fetch_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
            (Extensions.KHR.Physical_Device_Ray_Tracing_Position_Fetch_Features,
             Physical_Device_Ray_Tracing_Position_Fetch_Features_C,
             Physical_Device_Ray_Tracing_Position_Fetch_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Cooperative_Matrix_Properties_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                       (Extensions.KHR.Cooperative_Matrix_Properties,
                        Cooperative_Matrix_Properties_C,
                        Cooperative_Matrix_Properties_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Cooperative_Matrix_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                    (Extensions.KHR.Physical_Device_Cooperative_Matrix_Features,
                     Physical_Device_Cooperative_Matrix_Features_C,
                     Physical_Device_Cooperative_Matrix_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Cooperative_Matrix_Properties_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                  (Extensions.KHR.Physical_Device_Cooperative_Matrix_Properties,
                   Physical_Device_Cooperative_Matrix_Properties_C,
                   Physical_Device_Cooperative_Matrix_Properties_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Video_Decode_AV1_Capabilities_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                        (Extensions.KHR.Video_Decode_AV1_Capabilities,
                         Video_Decode_AV1_Capabilities_C,
                         Video_Decode_AV1_Capabilities_C_Access);
                begin
                    return Make_Struct(Next);
                end;
            when Physical_Device_Video_Maintenance_1_Features_Type =>
                declare
                    function Make_Struct is new Utilities.Make_Out_Struct
                   (Extensions.KHR.Physical_Device_Video_Maintenance_1_Features,
                    Physical_Device_Video_Maintenance_1_Features_C,
                    Physical_Device_Video_Maintenance_1_Features_C_Access);
                begin
                    return Make_Struct(Next);
                end;
        end case;
    end To_C;

    procedure To_Ada(Ada_Struct: in out Vulkan.Out_Structure'Class;
                     Next: in C.Out_Structure_C_Access) is
    begin
        case Out_Structure(Ada_Struct.Record_Type) is
            when Device_Group_Present_Capabilities_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Device_Group_Present_Capabilities_C_Access);
                begin
                    To_Ada(Extensions.KHR.Device_Group_Present_Capabilities
                            (Ada_Struct),
                           To_Access(Next).all);
                end;
            when Queue_Family_Query_Result_Status_Properties_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Queue_Family_Query_Result_Status_Properties_C_Access);
                begin
                    To_Ada
                     (Extensions.KHR.Queue_Family_Query_Result_Status_Properties
                        (Ada_Struct),
                      To_Access(Next).all);
                end;
            when Queue_Family_Video_Properties_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Queue_Family_Video_Properties_C_Access);
                begin
                    To_Ada(Extensions.KHR.Queue_Family_Video_Properties
                            (Ada_Struct),
                           To_Access(Next).all);
                end;
            when Video_Capabilities_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Video_Capabilities_C_Access);
                begin
                    To_Ada(Extensions.KHR.Video_Capabilities(Ada_Struct),
                           To_Access(Next).all);
                end;
            when Video_Format_Properties_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Video_Format_Properties_C_Access);
                begin
                    To_Ada(Extensions.KHR.Video_Format_Properties(Ada_Struct),
                           To_Access(Next).all);
                end;
            when Video_Session_Memory_Requirements_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Video_Session_Memory_Requirements_C_Access);
                begin
                    To_Ada(Extensions.KHR.Video_Session_Memory_Requirements
                            (Ada_Struct),
                           To_Access(Next).all);
                end;
            when Video_Decode_Capabilities_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Video_Decode_Capabilities_C_Access);
                begin
                    To_Ada(Extensions.KHR.Video_Decode_Capabilities(Ada_Struct),
                           To_Access(Next).all);
                end;
            when Video_Encode_H264_Capabilities_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Video_Encode_H264_Capabilities_C_Access);
                begin
                    To_Ada(Extensions.KHR.Video_Encode_H264_Capabilities
                            (Ada_Struct),
                           To_Access(Next).all);
                end;
            when Video_Encode_H264_Quality_Level_Properties_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Video_Encode_H264_Quality_Level_Properties_C_Access);
                begin
                To_Ada(Extensions.KHR.Video_Encode_H264_Quality_Level_Properties
                           (Ada_Struct),
                       To_Access(Next).all);
                end;
            when Video_Encode_H264_Session_Parameters_Feedback_Info_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                  (C.Out_Structure_C_Access,
                   Video_Encode_H264_Session_Parameters_Feedback_Info_C_Access);
                begin
        To_Ada(Extensions.KHR.Video_Encode_H264_Session_Parameters_Feedback_Info
                (Ada_Struct),
               To_Access(Next).all);
                end;
            when Video_Encode_H265_Capabilities_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Video_Encode_H265_Capabilities_C_Access);
                begin
                    To_Ada(Extensions.KHR.Video_Encode_H265_Capabilities
                        (Ada_Struct),
                    To_Access(Next).all);
                end;
            when Video_Encode_H265_Quality_Level_Properties_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Video_Encode_H265_Quality_Level_Properties_C_Access);
                begin
                To_Ada(Extensions.KHR.Video_Encode_H265_Quality_Level_Properties
                        (Ada_Struct),
                       To_Access(Next).all);
                end;
            when Video_Encode_H265_Session_Parameters_Feedback_Info_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                  (C.Out_Structure_C_Access,
                   Video_Encode_H265_Session_Parameters_Feedback_Info_C_Access);
                begin
                To_Ada
              (Extensions.KHR.Video_Encode_H265_Session_Parameters_Feedback_Info
                (Ada_Struct),
               To_Access(Next).all);
                end;
            when Video_Decode_H264_Capabilities_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Video_Decode_H264_Capabilities_C_Access);
                begin
                    To_Ada(Extensions.KHR.Video_Decode_H264_Capabilities
                            (Ada_Struct),
                           To_Access(Next).all);
                end;
            when Memory_FD_Properties_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Memory_FD_Properties_C_Access);
                begin
                    To_Ada(Extensions.KHR.Memory_FD_Properties(Ada_Struct),
                           To_Access(Next).all);
                end;
            when Shared_Present_Surface_Capabilities_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Shared_Present_Surface_Capabilities_C_Access);
                begin
                    To_Ada(Extensions.KHR.Shared_Present_Surface_Capabilities
                            (Ada_Struct),
                           To_Access(Next).all);
                end;
            when Physical_Device_Performance_Query_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Physical_Device_Performance_Query_Features_C_Access);
                begin
                    To_Ada
                      (Extensions.KHR.Physical_Device_Performance_Query_Features
                         (Ada_Struct),
                       To_Access(Next).all);
                end;
            when Physical_Device_Performance_Query_Properties_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Physical_Device_Performance_Query_Properties_C_Access);
                begin
                    To_Ada
                    (Extensions.KHR.Physical_Device_Performance_Query_Properties
                        (Ada_Struct),
                     To_Access(Next).all);
                end;
            when Performance_Counter_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Performance_Counter_C_Access);
                begin
                    To_Ada(Extensions.KHR.Performance_Counter(Ada_Struct),
                           To_Access(Next).all);
                end;
            when Performance_Counter_Description_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Performance_Counter_Description_C_Access);
                begin
                    To_Ada(Extensions.KHR.Performance_Counter_Description
                            (Ada_Struct),
                           To_Access(Next).all);
                end;
            when Surface_Capabilities_2_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Surface_Capabilities_2_C_Access);
                begin
                    To_Ada(Extensions.KHR.Surface_Capabilities_2(Ada_Struct),
                           To_Access(Next).all);
                end;
            when Surface_Format_2_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Surface_Format_2_C_Access);
                begin
                    To_Ada(Extensions.KHR.Surface_Format_2(Ada_Struct),
                           To_Access(Next).all);
                end;
            when Display_Properties_2_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Display_Properties_2_C_Access);
                begin
                    To_Ada(Extensions.KHR.Display_Properties_2(Ada_Struct),
                           To_Access(Next).all);
                end;
            when Display_Plane_Properties_2_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Display_Plane_Properties_2_C_Access);
                begin
                    To_Ada
                        (Extensions.KHR.Display_Plane_Properties_2(Ada_Struct),
                         To_Access(Next).all);
                end;
            when Display_Mode_Properties_2_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Display_Mode_Properties_2_C_Access);
                begin
                    To_Ada(Extensions.KHR.Display_Mode_Properties_2(Ada_Struct),
                           To_Access(Next).all);
                end;
            when Display_Plane_Capabilities_2_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Display_Plane_Capabilities_2_C_Access);
                begin
                    To_Ada
                        (Extensions.KHR.Display_Plane_Capabilities_2(Ada_Struct),
                         To_Access(Next).all);
                end;
            when Physical_Device_Shader_Clock_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Physical_Device_Shader_Clock_Features_C_Access);
                begin
                    To_Ada(Extensions.KHR.Physical_Device_Shader_Clock_Features
                            (Ada_Struct),
                           To_Access(Next).all);
                end;
            when Video_Decode_H265_Capabilities_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Video_Decode_H265_Capabilities_C_Access);
                begin
                    To_Ada(Extensions.KHR.Video_Decode_H265_Capabilities
                            (Ada_Struct),
                           To_Access(Next).all);
                end;
            when Physical_Device_Fragment_Shading_Rate_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                      (C.Out_Structure_C_Access,
                       Physical_Device_Fragment_Shading_Rate_Features_C_Access);
                begin
                    To_Ada
                  (Extensions.KHR.Physical_Device_Fragment_Shading_Rate_Features
                      (Ada_Struct),
                   To_Access(Next).all);
                end;
            when Physical_Device_Fragment_Shading_Rate_Properties_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                    (C.Out_Structure_C_Access,
                     Physical_Device_Fragment_Shading_Rate_Properties_C_Access);
                begin
                    To_Ada
                (Extensions.KHR.Physical_Device_Fragment_Shading_Rate_Properties
                    (Ada_Struct),
                 To_Access(Next).all);
                end;
            when Physical_Device_Fragment_Shading_Rate_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Physical_Device_Fragment_Shading_Rate_C_Access);
                begin
                    To_Ada(Extensions.KHR.Physical_Device_Fragment_Shading_Rate
                            (Ada_Struct),
                           To_Access(Next).all);
                end;
            when Physical_Device_Present_Wait_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Physical_Device_Present_Wait_Features_C_Access);
                begin
                    To_Ada(Extensions.KHR.Physical_Device_Present_Wait_Features
                            (Ada_Struct),
                           To_Access(Next).all);
                end;
            when Physical_Device_Shader_Quad_Control_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Physical_Device_Shader_Quad_Control_Features_C_Access);
                begin
                    To_Ada
                    (Extensions.KHR.Physical_Device_Shader_Quad_Control_Features
                        (Ada_Struct),
                     To_Access(Next).all);
                end;
            when Physical_Device_Pipeline_Executable_Properties_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
             (C.Out_Structure_C_Access,
              Physical_Device_Pipeline_Executable_Properties_Features_C_Access);
                begin
                    To_Ada
         (Extensions.KHR.Physical_Device_Pipeline_Executable_Properties_Features
             (Ada_Struct),
          To_Access(Next).all);
                end;
            when Pipeline_Executable_Properties_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Pipeline_Executable_Properties_C_Access);
                begin
                    To_Ada(Extensions.KHR.Pipeline_Executable_Properties
                            (Ada_Struct),
                           To_Access(Next).all);
                end;
            when Pipeline_Executable_Statistic_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Pipeline_Executable_Statistic_C_Access);
                begin
                    To_Ada(Extensions.KHR.Pipeline_Executable_Statistic
                            (Ada_Struct),
                           To_Access(Next).all);
                end;
            when Pipeline_Executable_Internal_Representation_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Pipeline_Executable_Internal_Representation_C_Access);
                begin
                    To_Ada
                     (Extensions.KHR.Pipeline_Executable_Internal_Representation
                         (Ada_Struct),
                      To_Access(Next).all);
                end;
            when Physical_Device_Present_ID_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Physical_Device_Present_ID_Features_C_Access);
                begin
                    To_Ada(Extensions.KHR.Physical_Device_Present_ID_Features
                            (Ada_Struct),
                           To_Access(Next).all);
                end;
            when Video_Encode_Capabilities_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Video_Encode_Capabilities_C_Access);
                begin
                    To_Ada(Extensions.KHR.Video_Encode_Capabilities(Ada_Struct),
                           To_Access(Next).all);
                end;
            when Video_Encode_Quality_Level_Properties_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Video_Encode_Quality_Level_Properties_C_Access);
                begin
                    To_Ada(Extensions.KHR.Video_Encode_Quality_Level_Properties
                            (Ada_Struct),
                           To_Access(Next).all);
                end;
            when Video_Encode_Session_Parameters_Feedback_Info_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Video_Encode_Session_Parameters_Feedback_Info_C_Access);
                begin
             To_Ada(Extensions.KHR.Video_Encode_Session_Parameters_Feedback_Info
                     (Ada_Struct),
                    To_Access(Next).all);
                end;
            when Physical_Device_Fragment_Shader_Barycentric_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                (C.Out_Structure_C_Access,
                 Physical_Device_Fragment_Shader_Barycentric_Features_C_Access);
                begin
                    To_Ada
            (Extensions.KHR.Physical_Device_Fragment_Shader_Barycentric_Features
                (Ada_Struct),
             To_Access(Next).all);
                end;
            when Physical_Device_Fragment_Shader_Barycentric_Properties_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
              (C.Out_Structure_C_Access,
               Physical_Device_Fragment_Shader_Barycentric_Properties_C_Access);
                begin
                    To_Ada
          (Extensions.KHR.Physical_Device_Fragment_Shader_Barycentric_Properties
              (Ada_Struct),
           To_Access(Next).all);
                end;
            when
           Physical_Device_Shader_Subgroup_Uniform_Control_Flow_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
       (C.Out_Structure_C_Access,
        Physical_Device_Shader_Subgroup_Uniform_Control_Flow_Features_C_Access);
                begin
                    To_Ada
   (Extensions.KHR.Physical_Device_Shader_Subgroup_Uniform_Control_Flow_Features
       (Ada_Struct),
    To_Access(Next).all);
                end;
            when
               Physical_Device_Workgroup_Memory_Explicit_Layout_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
           (C.Out_Structure_C_Access,
            Physical_Device_Workgroup_Memory_Explicit_Layout_Features_C_Access);
                begin
                    To_Ada
       (Extensions.KHR.Physical_Device_Workgroup_Memory_Explicit_Layout_Features
           (Ada_Struct),
        To_Access(Next).all);
                end;
            when Physical_Device_Ray_Tracing_Maintenance_1_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                  (C.Out_Structure_C_Access,
                   Physical_Device_Ray_Tracing_Maintenance_1_Features_C_Access);
                begin
                    To_Ada
              (Extensions.KHR.Physical_Device_Ray_Tracing_Maintenance_1_Features
                  (Ada_Struct),
               To_Access(Next).all);
                end;
            when Physical_Device_Shader_Maximal_Reconvergence_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
               (C.Out_Structure_C_Access,
                Physical_Device_Shader_Maximal_Reconvergence_Features_C_Access);
                begin
                    To_Ada
           (Extensions.KHR.Physical_Device_Shader_Maximal_Reconvergence_Features
               (Ada_Struct),
            To_Access(Next).all);
                end;
            when Physical_Device_Ray_Tracing_Position_Fetch_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                 (C.Out_Structure_C_Access,
                  Physical_Device_Ray_Tracing_Position_Fetch_Features_C_Access);
                begin
                    To_Ada
             (Extensions.KHR.Physical_Device_Ray_Tracing_Position_Fetch_Features
                 (Ada_Struct),
              To_Access(Next).all);
                end;
            when Cooperative_Matrix_Properties_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                       (C.Out_Structure_C_Access,
                        Cooperative_Matrix_Properties_C_Access);
                begin
                    To_Ada(Extensions.KHR.Cooperative_Matrix_Properties
                            (Ada_Struct),
                           To_Access(Next).all);
                end;
            when Physical_Device_Cooperative_Matrix_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                       (C.Out_Structure_C_Access,
                        Physical_Device_Cooperative_Matrix_Features_C_Access);
                begin
                    To_Ada
                     (Extensions.KHR.Physical_Device_Cooperative_Matrix_Features
                         (Ada_Struct),
                      To_Access(Next).all);
                end;
            when Physical_Device_Cooperative_Matrix_Properties_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                       (C.Out_Structure_C_Access,
                        Physical_Device_Cooperative_Matrix_Properties_C_Access);
                begin
                    To_Ada
                   (Extensions.KHR.Physical_Device_Cooperative_Matrix_Properties
                       (Ada_Struct),
                    To_Access(Next).all);
                end;
            when Video_Decode_AV1_Capabilities_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Video_Decode_AV1_Capabilities_C_Access);
                begin
                    To_Ada(Extensions.KHR.Video_Decode_AV1_Capabilities
                            (Ada_Struct),
                           To_Access(Next).all);
                end;
            when Physical_Device_Video_Maintenance_1_Features_Type =>
                declare
                    function To_Access is new Ada.Unchecked_Conversion
                        (C.Out_Structure_C_Access,
                         Physical_Device_Video_Maintenance_1_Features_C_Access);
                begin
                    To_Ada
                    (Extensions.KHR.Physical_Device_Video_Maintenance_1_Features
                        (Ada_Struct),
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
            when Swapchain_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Swapchain_Create_Info_C,
                         Swapchain_Create_Info_C_Access,
                         Free);
                begin
                    Free_Struct(Next);
                end;
            when Present_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Present_Info_C,
                         Present_Info_C_Access,
                         Free);
                begin
                    Free_Struct(Next);
                end;
            when Image_Swapchain_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Image_Swapchain_Create_Info_C,
                         Image_Swapchain_Create_Info_C_Access,
                         Free);
                begin
                    Free_Struct(Next);
                end;
            when Bind_Image_Memory_Swapchain_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Bind_Image_Memory_Swapchain_Info_C,
                         Bind_Image_Memory_Swapchain_Info_C_Access,
                         Free);
                begin
                    Free_Struct(Next);
                end;
            when Acquire_Next_Image_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Acquire_Next_Image_Info_C,
                         Acquire_Next_Image_Info_C_Access,
                         Free);
                begin
                    Free_Struct(Next);
                end;
            when Device_Group_Present_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Device_Group_Present_Info_C,
                         Device_Group_Present_Info_C_Access,
                         Free);
                begin
                    Free_Struct(Next);
                end;
            when Device_Group_Swapchain_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Device_Group_Swapchain_Create_Info_C,
                         Device_Group_Swapchain_Create_Info_C_Access,
                         Free);
                begin
                    Free_Struct(Next);
                end;
            when Display_Mode_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Display_Mode_Create_Info_C,
                         Display_Mode_Create_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Display_Surface_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Display_Surface_Create_Info_C,
                         Display_Surface_Create_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Display_Present_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Display_Present_Info_C,
                         Display_Present_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Profile_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Video_Profile_Info_C,
                         Video_Profile_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Profile_List_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Video_Profile_List_Info_C,
                         Video_Profile_List_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Video_Format_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Physical_Device_Video_Format_Info_C,
                         Physical_Device_Video_Format_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Picture_Resource_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Video_Picture_Resource_Info_C,
                         Video_Picture_Resource_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Reference_Slot_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Video_Reference_Slot_Info_C,
                         Video_Reference_Slot_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Bind_Video_Session_Memory_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Bind_Video_Session_Memory_Info_C,
                         Bind_Video_Session_Memory_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Session_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Video_Session_Create_Info_C,
                         Video_Session_Create_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Session_Parameters_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Video_Session_Parameters_Create_Info_C,
                         Video_Session_Parameters_Create_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Session_Parameters_Update_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Video_Session_Parameters_Update_Info_C,
                         Video_Session_Parameters_Update_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Begin_Coding_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Video_Begin_Coding_Info_C,
                         Video_Begin_Coding_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_End_Coding_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Video_End_Coding_Info_C,
                         Video_End_Coding_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Coding_Control_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Video_Coding_Control_Info_C,
                         Video_Coding_Control_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Decode_Usage_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Video_Decode_Usage_Info_C,
                         Video_Decode_Usage_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Decode_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Video_Decode_Info_C, Video_Decode_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Encode_H264_Session_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Video_Encode_H264_Session_Create_Info_C,
                         Video_Encode_H264_Session_Create_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Encode_H264_Session_Parameters_Add_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                       (Video_Encode_H264_Session_Parameters_Add_Info_C,
                        Video_Encode_H264_Session_Parameters_Add_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Encode_H264_Session_Parameters_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                    (Video_Encode_H264_Session_Parameters_Create_Info_C,
                     Video_Encode_H264_Session_Parameters_Create_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Encode_H264_Session_Parameters_Get_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                       (Video_Encode_H264_Session_Parameters_Get_Info_C,
                        Video_Encode_H264_Session_Parameters_Get_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Encode_H264_Nalu_Slice_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Video_Encode_H264_Nalu_Slice_Info_C,
                         Video_Encode_H264_Nalu_Slice_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Encode_H264_Picture_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Video_Encode_H264_Picture_Info_C,
                         Video_Encode_H264_Picture_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Encode_H264_DPB_Slot_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Video_Encode_H264_DPB_Slot_Info_C,
                         Video_Encode_H264_DPB_Slot_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Encode_H264_Profile_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Video_Encode_H264_Profile_Info_C,
                         Video_Encode_H264_Profile_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Encode_H264_Rate_Control_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Video_Encode_H264_Rate_Control_Info_C,
                         Video_Encode_H264_Rate_Control_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Encode_H264_Rate_Control_Layer_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Video_Encode_H264_Rate_Control_Layer_Info_C,
                         Video_Encode_H264_Rate_Control_Layer_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Encode_H264_GOP_Remaining_Frame_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Video_Encode_H264_GOP_Remaining_Frame_Info_C,
                         Video_Encode_H264_GOP_Remaining_Frame_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Encode_H265_Session_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Video_Encode_H265_Session_Create_Info_C,
                         Video_Encode_H265_Session_Create_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Encode_H265_Session_Parameters_Add_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                       (Video_Encode_H265_Session_Parameters_Add_Info_C,
                        Video_Encode_H265_Session_Parameters_Add_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Encode_H265_Session_Parameters_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                    (Video_Encode_H265_Session_Parameters_Create_Info_C,
                     Video_Encode_H265_Session_Parameters_Create_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Encode_H265_Session_Parameters_Get_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                       (Video_Encode_H265_Session_Parameters_Get_Info_C,
                        Video_Encode_H265_Session_Parameters_Get_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Encode_H265_Nalu_Slice_Segment_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Video_Encode_H265_Nalu_Slice_Segment_Info_C,
                         Video_Encode_H265_Nalu_Slice_Segment_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Encode_H265_Picture_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Video_Encode_H265_Picture_Info_C,
                         Video_Encode_H265_Picture_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Encode_H265_DPB_Slot_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Video_Encode_H265_DPB_Slot_Info_C,
                         Video_Encode_H265_DPB_Slot_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Encode_H265_Profile_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Video_Encode_H265_Profile_Info_C,
                         Video_Encode_H265_Profile_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Encode_H265_Rate_Control_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Video_Encode_H265_Rate_Control_Info_C,
                         Video_Encode_H265_Rate_Control_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Encode_H265_Rate_Control_Layer_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Video_Encode_H265_Rate_Control_Layer_Info_C,
                         Video_Encode_H265_Rate_Control_Layer_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Encode_H265_GOP_Remaining_Frame_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Video_Encode_H265_GOP_Remaining_Frame_Info_C,
                         Video_Encode_H265_GOP_Remaining_Frame_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Decode_H264_Profile_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Video_Decode_H264_Profile_Info_C,
                         Video_Decode_H264_Profile_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Decode_H264_Session_Parameters_Add_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                       (Video_Decode_H264_Session_Parameters_Add_Info_C,
                        Video_Decode_H264_Session_Parameters_Add_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Decode_H264_Session_Parameters_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                    (Video_Decode_H264_Session_Parameters_Create_Info_C,
                     Video_Decode_H264_Session_Parameters_Create_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Decode_H264_Picture_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Video_Decode_H264_Picture_Info_C,
                         Video_Decode_H264_Picture_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Decode_H264_DPB_Slot_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Video_Decode_H264_DPB_Slot_Info_C,
                         Video_Decode_H264_DPB_Slot_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Rendering_Fragment_Shading_Rate_Attachment_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                     (Rendering_Fragment_Shading_Rate_Attachment_Info_C,
                      Rendering_Fragment_Shading_Rate_Attachment_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Import_Memory_FD_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Import_Memory_FD_Info_C,
                         Import_Memory_FD_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Memory_Get_FD_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Memory_Get_FD_Info_C,
                         Memory_Get_FD_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Import_Semaphore_FD_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Import_Semaphore_FD_Info_C,
                         Import_Semaphore_FD_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Semaphore_Get_FD_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Semaphore_Get_FD_Info_C,
                         Semaphore_Get_FD_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Present_Regions_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Present_Regions_C,
                         Present_Regions_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Import_Fence_FD_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Import_Fence_FD_Info_C,
                         Import_Fence_FD_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Fence_Get_FD_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Fence_Get_FD_Info_C,
                         Fence_Get_FD_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Query_Pool_Performance_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Query_Pool_Performance_Create_Info_C,
                         Query_Pool_Performance_Create_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Acquire_Profiling_Lock_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Acquire_Profiling_Lock_Info_C,
                         Acquire_Profiling_Lock_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Performance_Query_Submit_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Performance_Query_Submit_Info_C,
                         Performance_Query_Submit_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Surface_Info_2_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Physical_Device_Surface_Info_2_C,
                         Physical_Device_Surface_Info_2_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Display_Plane_Info_2_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Display_Plane_Info_2_C, Display_Plane_Info_2_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Decode_H265_Profile_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Video_Decode_H265_Profile_Info_C,
                         Video_Decode_H265_Profile_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Decode_H265_Session_Parameters_Add_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Video_Decode_H265_Session_Parameters_Add_Info_C,
                         Video_Decode_H265_Session_Parameters_Add_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Decode_H265_Session_Parameters_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                    (Video_Decode_H265_Session_Parameters_Create_Info_C,
                     Video_Decode_H265_Session_Parameters_Create_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Decode_H265_Picture_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Video_Decode_H265_Picture_Info_C,
                         Video_Decode_H265_Picture_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Decode_H265_DPB_Slot_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Video_Decode_H265_DPB_Slot_Info_C,
                         Video_Decode_H265_DPB_Slot_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Fragment_Shading_Rate_Attachment_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Fragment_Shading_Rate_Attachment_Info_C,
                         Fragment_Shading_Rate_Attachment_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Pipeline_Fragment_Shading_Rate_State_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                    (Pipeline_Fragment_Shading_Rate_State_Create_Info_C,
                     Pipeline_Fragment_Shading_Rate_State_Create_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Surface_Protected_Capabilities_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Surface_Protected_Capabilities_C,
                         Surface_Protected_Capabilities_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Pipeline_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Pipeline_Info_C, Pipeline_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Pipeline_Executable_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Pipeline_Executable_Info_C,
                         Pipeline_Executable_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Pipeline_Library_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Pipeline_Library_Create_Info_C,
                         Pipeline_Library_Create_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Present_ID_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Present_ID_C, Present_ID_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Encode_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Video_Encode_Info_C, Video_Encode_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Query_Pool_Video_Encode_Feedback_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Query_Pool_Video_Encode_Feedback_Create_Info_C,
                         Query_Pool_Video_Encode_Feedback_Create_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Encode_Usage_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Video_Encode_Usage_Info_C,
                         Video_Encode_Usage_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Encode_Rate_Control_Layer_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Video_Encode_Rate_Control_Layer_Info_C,
                         Video_Encode_Rate_Control_Layer_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Encode_Rate_Control_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Video_Encode_Rate_Control_Info_C,
                         Video_Encode_Rate_Control_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Video_Encode_Quality_Level_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                     (Physical_Device_Video_Encode_Quality_Level_Info_C,
                      Physical_Device_Video_Encode_Quality_Level_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Encode_Quality_Level_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Video_Encode_Quality_Level_Info_C,
                         Video_Encode_Quality_Level_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Encode_Session_Parameters_Get_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Video_Encode_Session_Parameters_Get_Info_C,
                         Video_Encode_Session_Parameters_Get_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Decode_AV1_Profile_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Video_Decode_AV1_Profile_Info_C,
                         Video_Decode_AV1_Profile_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Decode_AV1_Session_Parameters_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                     (Video_Decode_AV1_Session_Parameters_Create_Info_C,
                      Video_Decode_AV1_Session_Parameters_Create_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Decode_AV1_Picture_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Video_Decode_AV1_Picture_Info_C,
                          Video_Decode_AV1_Picture_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Decode_AV1_DPB_Slot_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Video_Decode_AV1_DPB_Slot_Info_C,
                          Video_Decode_AV1_DPB_Slot_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Inline_Query_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Video_Inline_Query_Info_C,
                         Video_Inline_Query_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Calibrated_Timestamp_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Calibrated_Timestamp_Info_C,
                         Calibrated_Timestamp_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Set_Descriptor_Buffer_Offsets_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Set_Descriptor_Buffer_Offsets_Info_C,
                         Set_Descriptor_Buffer_Offsets_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Bind_Descriptor_Buffer_Embedded_Samplers_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                       (Bind_Descriptor_Buffer_Embedded_Samplers_Info_C,
                        Bind_Descriptor_Buffer_Embedded_Samplers_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Xlib_Surface_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Xlib_Surface_Create_Info_C,
                         Xlib_Surface_Create_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Xcb_Surface_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Xcb_Surface_Create_Info_C,
                         Xcb_Surface_Create_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Wayland_Surface_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Wayland_Surface_Create_Info_C,
                         Wayland_Surface_Create_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Win32_Surface_Create_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_In_Struct
                        (Win32_Surface_Create_Info_C,
                         Win32_Surface_Create_Info_C_Access);
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
            when Device_Group_Present_Capabilities_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Device_Group_Present_Capabilities_C,
                         Device_Group_Present_Capabilities_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Queue_Family_Query_Result_Status_Properties_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Queue_Family_Query_Result_Status_Properties_C,
                         Queue_Family_Query_Result_Status_Properties_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Queue_Family_Video_Properties_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Queue_Family_Video_Properties_C,
                         Queue_Family_Video_Properties_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Capabilities_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Video_Capabilities_C,
                         Video_Capabilities_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Format_Properties_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Video_Format_Properties_C,
                         Video_Format_Properties_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Session_Memory_Requirements_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Video_Session_Memory_Requirements_C,
                         Video_Session_Memory_Requirements_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Decode_Capabilities_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Video_Decode_Capabilities_C,
                         Video_Decode_Capabilities_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Encode_H264_Capabilities_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Video_Encode_H264_Capabilities_C,
                         Video_Encode_H264_Capabilities_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Encode_H264_Quality_Level_Properties_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Video_Encode_H264_Quality_Level_Properties_C,
                         Video_Encode_H264_Quality_Level_Properties_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Encode_H264_Session_Parameters_Feedback_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                  (Video_Encode_H264_Session_Parameters_Feedback_Info_C,
                   Video_Encode_H264_Session_Parameters_Feedback_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Encode_H265_Capabilities_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Video_Encode_H265_Capabilities_C,
                         Video_Encode_H265_Capabilities_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Encode_H265_Quality_Level_Properties_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Video_Encode_H265_Quality_Level_Properties_C,
                         Video_Encode_H265_Quality_Level_Properties_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Encode_H265_Session_Parameters_Feedback_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                  (Video_Encode_H265_Session_Parameters_Feedback_Info_C,
                   Video_Encode_H265_Session_Parameters_Feedback_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Decode_H264_Capabilities_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Video_Decode_H264_Capabilities_C,
                         Video_Decode_H264_Capabilities_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Memory_FD_Properties_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Memory_FD_Properties_C,
                         Memory_FD_Properties_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Shared_Present_Surface_Capabilities_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Shared_Present_Surface_Capabilities_C,
                         Shared_Present_Surface_Capabilities_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Performance_Query_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Physical_Device_Performance_Query_Features_C,
                         Physical_Device_Performance_Query_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Performance_Query_Properties_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Physical_Device_Performance_Query_Properties_C,
                         Physical_Device_Performance_Query_Properties_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Performance_Counter_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Performance_Counter_C, Performance_Counter_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Performance_Counter_Description_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Performance_Counter_Description_C,
                         Performance_Counter_Description_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Surface_Capabilities_2_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Surface_Capabilities_2_C,
                         Surface_Capabilities_2_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Surface_Format_2_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Surface_Format_2_C, Surface_Format_2_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Display_Properties_2_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Display_Properties_2_C,
                         Display_Properties_2_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Display_Plane_Properties_2_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Display_Plane_Properties_2_C,
                         Display_Plane_Properties_2_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Display_Mode_Properties_2_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Display_Mode_Properties_2_C,
                         Display_Mode_Properties_2_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Display_Plane_Capabilities_2_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Display_Plane_Capabilities_2_C,
                         Display_Plane_Capabilities_2_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Shader_Clock_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Physical_Device_Shader_Clock_Features_C,
                         Physical_Device_Shader_Clock_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Decode_H265_Capabilities_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Video_Decode_H265_Capabilities_C,
                         Video_Decode_H265_Capabilities_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Fragment_Shading_Rate_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                      (Physical_Device_Fragment_Shading_Rate_Features_C,
                       Physical_Device_Fragment_Shading_Rate_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Fragment_Shading_Rate_Properties_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                    (Physical_Device_Fragment_Shading_Rate_Properties_C,
                     Physical_Device_Fragment_Shading_Rate_Properties_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Fragment_Shading_Rate_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Physical_Device_Fragment_Shading_Rate_C,
                         Physical_Device_Fragment_Shading_Rate_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Shader_Quad_Control_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Physical_Device_Shader_Quad_Control_Features_C,
                         Physical_Device_Shader_Quad_Control_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Present_Wait_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Physical_Device_Present_Wait_Features_C,
                         Physical_Device_Present_Wait_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Pipeline_Executable_Properties_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
             (Physical_Device_Pipeline_Executable_Properties_Features_C,
              Physical_Device_Pipeline_Executable_Properties_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Pipeline_Executable_Properties_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Pipeline_Executable_Properties_C,
                         Pipeline_Executable_Properties_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Pipeline_Executable_Statistic_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Pipeline_Executable_Statistic_C,
                         Pipeline_Executable_Statistic_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Pipeline_Executable_Internal_Representation_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Pipeline_Executable_Internal_Representation_C,
                         Pipeline_Executable_Internal_Representation_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Present_ID_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Physical_Device_Present_ID_Features_C,
                         Physical_Device_Present_ID_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Encode_Capabilities_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Video_Encode_Capabilities_C,
                         Video_Encode_Capabilities_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Encode_Quality_Level_Properties_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Video_Encode_Quality_Level_Properties_C,
                         Video_Encode_Quality_Level_Properties_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Encode_Session_Parameters_Feedback_Info_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                       (Video_Encode_Session_Parameters_Feedback_Info_C,
                        Video_Encode_Session_Parameters_Feedback_Info_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Fragment_Shader_Barycentric_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                (Physical_Device_Fragment_Shader_Barycentric_Features_C,
                 Physical_Device_Fragment_Shader_Barycentric_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Fragment_Shader_Barycentric_Properties_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
              (Physical_Device_Fragment_Shader_Barycentric_Properties_C,
               Physical_Device_Fragment_Shader_Barycentric_Properties_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when
           Physical_Device_Shader_Subgroup_Uniform_Control_Flow_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
       (Physical_Device_Shader_Subgroup_Uniform_Control_Flow_Features_C,
        Physical_Device_Shader_Subgroup_Uniform_Control_Flow_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when
               Physical_Device_Workgroup_Memory_Explicit_Layout_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
           (Physical_Device_Workgroup_Memory_Explicit_Layout_Features_C,
            Physical_Device_Workgroup_Memory_Explicit_Layout_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Ray_Tracing_Maintenance_1_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                  (Physical_Device_Ray_Tracing_Maintenance_1_Features_C,
                   Physical_Device_Ray_Tracing_Maintenance_1_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Shader_Maximal_Reconvergence_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
               (Physical_Device_Shader_Maximal_Reconvergence_Features_C,
                Physical_Device_Shader_Maximal_Reconvergence_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Ray_Tracing_Position_Fetch_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                 (Physical_Device_Ray_Tracing_Position_Fetch_Features_C,
                  Physical_Device_Ray_Tracing_Position_Fetch_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Cooperative_Matrix_Properties_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Cooperative_Matrix_Properties_C,
                         Cooperative_Matrix_Properties_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Cooperative_Matrix_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Physical_Device_Cooperative_Matrix_Features_C,
                         Physical_Device_Cooperative_Matrix_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Cooperative_Matrix_Properties_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                       (Physical_Device_Cooperative_Matrix_Properties_C,
                        Physical_Device_Cooperative_Matrix_Properties_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Video_Decode_AV1_Capabilities_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Video_Decode_AV1_Capabilities_C,
                         Video_Decode_AV1_Capabilities_C_Access);
                begin
                    Free_Struct(Next);
                end;
            when Physical_Device_Video_Maintenance_1_Features_Type =>
                declare
                    procedure Free_Struct is new Utilities.Free_Out_Struct
                        (Physical_Device_Video_Maintenance_1_Features_C,
                         Physical_Device_Video_Maintenance_1_Features_C_Access);
                begin
                    Free_Struct(Next);
                end;
        end case;
    end Free;
end Vulkan.C_KHR;

