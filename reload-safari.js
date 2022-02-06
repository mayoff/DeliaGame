#!/usr/bin/osascript -lJavaScript

(() => {
    const targetUrl = 'http://eos.dqd.com/~mayoff/DeliaGame/';
    const safari = Application('Safari');
    safari.activate();

    for (const w of safari.windows()) {
        // Inspectors don't have tabs.
        if (w.currentTab() === null) {
            continue;
        }

        for (const t of w.tabs()) {
            if (t.url() === targetUrl) {
                safari.doJavaScript('location.reload()', { in: t });

                // Activate the tab.
                w.currentTab = t;

                // Raise the window.
                w.index = 1;

                return;
            }
        }
    }

    if (safari.windows.length < 1) {
        let w = safari.Window();
        safari.windows.push(w);
    }

    let w = safari.windows[0];
    let t = safari.Tab({ url: targetUrl });
    w.tabs.push(t);
    w.currentTab = t;
    w.index = 1;
})();
