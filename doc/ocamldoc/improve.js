$(document).ready(
    function() {
        var indiv = function (divclass, contents) {
            return '<div class="' + divclass + '">' + contents + '</div>';
        };

        var sh_button = '<a class="showhide bbutton" href=""><span></span></a>';

        // Make all top-level anchors menu links
        $('body > a').addClass( "menu_item" );

        $('td.typefieldcomment').replaceWith(
            function() {
                cts = $(this).text();
                switch (cts) {
                case "(*":
                case "*)":
                    return "";
                default:
                    return sh_button + indiv("moca-doc", $(this).html());
                }
            });

        $('div.info').replaceWith(
            function() {
                return sh_button + indiv("moca-doc", $(this).html());
            }
        );


        $('td').replaceWith(
            function() {
                return $(this).html();
            });

        $('tr').replaceWith(
            function() {
                return indiv("moca-gen", indiv("moca-tr", $(this).html()));
            });

        $('table.typetable').replaceWith(
            function() {
                return indiv("moca-type", $(this).html());
            });

        $('table').replaceWith(
            function() {
                return indiv("moca-index", $(this).html());
            });

        $('br').replaceWith(
            function() {
                var next_tag = $(this).next()[0];

                if (typeof next_tag !== 'undefined') {
                    switch (next_tag.nodeName.toLowerCase()) {
                    case "code":
                    case "pre":
                        return indiv('vspacer','');
                    default:
                        return "";
                    }
                }
            }
        );


        $('pre').replaceWith(
            function() {
                return (indiv("moca-pre", $(this).html()));
            }
        );

        var cclick = function (show, hide) {
            $("a.showhide > span").text(hide);
            $("a.showhide").next().show();
            $("a.showhide").toggle(
                function () {
                    $(this).children("span").text(show);
                    $(this).next().hide();
                },
                function() {
                    $(this).children("span").text(hide);
                    $(this).next().show();
                }
            );
        };

        cclick("Show doc","Hide doc");

    }
);