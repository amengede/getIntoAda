with Logger;
with Vulkan.Physical_Devices;
with Vulkan.Extensions;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

package body Devices is

    procedure Choose_Physical_Device (Physical_Device : out Vulkan.Physical_Device;
                                     Instance : Vulkan.Instance) is
        Available_Devices : Vulkan.Physical_Device_Vectors.Vector :=
            Vulkan.Physical_Devices.Enumerate (Instance);
        Requested_Extensions : Vulkan.String_Vectors.Vector;
    begin
        Requested_Extensions.Append (Vulkan.Extensions.Swapchain);
        for Device of Available_Devices loop
            Logger.Print (Device);
            if Supports (Device,  Requested_Extensions) then
                Physical_Device := Device;
                return;
            end if;
        end loop;
    end Choose_Physical_Device;

    function Supports (Device : Vulkan.Physical_Device;
                       Requested_Extensions : Vulkan.String_Vectors.Vector)
        return Boolean is
        Supported : Boolean;
        Supported_Extensions : Vulkan.Extension_Properties_Vectors.Vector :=
            Vulkan.Physical_Devices.Enumerate_Extension_Properties (Device);
    begin
        for Extension_Name of Requested_Extensions loop
            Supported := False;
            for Extension of Supported_Extensions loop
                if Extension_Name = Extension.Name then
                    supported := True;
                    goto Continue;
                end if;
            end loop;
            if not Supported then
                return False;
            end if;
    <<Continue>>
        end loop;
        return True;
    end Supports;
end Devices;
