#!/usr/bin/osascript -lJavaScript

(() => {
    const targetUrl = 'http://eos.dqd.com/~mayoff/DeliaGame/';
    const safari = Application('Safari');
	safari.activate();

    for (const w of safari.windows()) {
    	for (const t of w.tabs()) {
    		if (t.url() == targetUrl) {
    			didFindTab = true;

				safari.doJavaScript('location.reload()', { in: t });

    			// Activate the tab.
    			w.currentTab = t;

    			// Raise the window.
    			w.index = 1;

				return;
    		}
    	}
    }
})();
