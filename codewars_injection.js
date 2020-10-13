function registerBlurBlockers() {
    textareas = Array.from(document.getElementsByTagName("textarea"));
    if (textareas.length < 2) setTimeout(registerBlurBlockers, 50);
    else {
        textareas.forEach(function(el) {
            el.addEventListener('focusout', function() {
                (event.relatedTarget || el).focus();
            })
        });
        console.log('Registered focusout blockers for textareas');
    }
}
setTimeout(registerBlurBlockers, 50);

