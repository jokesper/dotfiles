local json = ...

return
{
	privacy = {
		['resistFingerprinting.letterboxing'] = false,
		['globalprivacycontrol.enabled'] = true,
	},
	browser = {
		['urlbar.placeholderName'] = '<...>',
		['urlbar.placeholderName.private'] = '<...>',
		['translations.panelShown'] = true,
		['uiCustomization.state'] = json.encode {
			placements = {
				['widget-overflow-fixed-list'] = {},
				['unified-extensions-area'] = {},
				['nav-bar'] = {
					'back-button',
					'forward-button',
					'stop-reload-button',
					'customizableui-special-spring1',
					'urlbar-container',
					'customizableui-special-spring2',
					'downloads-button',
					'unified-extensions-button',
				},
				['toolbar-menubar'] = { 'menubar-items' },
				TabsToolbar = { 'tabbrowser-tabs', 'new-tab-button', 'alltabs-button' },
				PersonalToolbar = { 'personal-bookmarks' }
			},
			seen = {
				'save-to-pocket-button',
				'developer-button',
			},
			dirtyAreaCache = {
				'nav-bar',
				'PersonalToolbar',
				'unified-extensions-area',
				'toolbar-menubar',
				'TabsToolbar',
			},
			currentVersion = 19,
			newElementCount = 3,
		}
	},
	['extensions.webextensions.restrictedDomains'] = '';
	toolkit = {
		['legacyUserProfileCustomizations.stylesheets'] = true
	},
	['full-screen-api.ignore-widgets'] = true,
	pdfjs = {
		sidebarViewOnLoad = 0,
	},
}
