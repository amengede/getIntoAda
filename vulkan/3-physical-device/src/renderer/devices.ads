with Vulkan;

package Devices is

    ---------------------------------------------------------------------------
    --  Choose a physical device
    --  @param Physical_Device Device to populate with choice
    --  @param Instance Vulkan instance to support
    ---------------------------------------------------------------------------
    procedure Choose_Physical_Device (Physical_Device : out Vulkan.Physical_Device;
                                     Instance : Vulkan.Instance);

private

    ---------------------------------------------------------------------------
    --  Does a physical device support the requested list of extensions?
    --  @param Device Physical device to test
    --  @param Requested_Extensions List of requested physical device
    --  extensions
    --  @return Whether the physical device can support the extensions
    ---------------------------------------------------------------------------
    function Supports (Device : Vulkan.Physical_Device;
                       Requested_Extensions : Vulkan.String_Vectors.Vector)
        return Boolean;
end Devices;
