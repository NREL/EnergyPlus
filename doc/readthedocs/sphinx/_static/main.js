(function ($) {
    function listFilter(header) { // header is any element, list is an unordered list
        // create and add the filter form to the header
        const form = $("<form>").attr({"class": "filterform", "action": "#"});
        const input = $("<input>").attr({"class": "filterinput", "type": "text"});
        $(form).append(input).appendTo(header);

        $(input)
            .change(function () {
                const filter = $(this).val();
                const listItems = $(".special");
                listItems.each(function (idx, li) {
                    const thisOne = $(li)[0];
                    if (filter === "") {
                        $(li).show();
                    } else if (thisOne.children[0].innerText.includes(filter)) {
                        $(li).show();
                    } else {
                        $(li).hide();
                    }
                });
                return false;
            })
            .keyup(function () {
                // fire the above change event after every letter
                $(this).change();
            });
    }
    $(function () {
        listFilter($("#header"));
    });
}(jQuery));
