require 'cloak'.setup{
	patterns = {
		{
			file_pattern = '*wpa_supplicant*.conf',
			-- FIXME: only hide value
			cloak_pattern = 'psk=(.+)',
		},
	},
}
