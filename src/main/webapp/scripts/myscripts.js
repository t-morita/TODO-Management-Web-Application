function updateTodoListTable() {
    $('#todolist_table').trigger("update");
}

function openConfirmLogoutDialog(title) {
    $('#confirm_logout_dialog').dialog({
            autoOpen: false,
            height: 200,
            width: 300,
            modal: true,
            title: title,
            overlay: {
                backgroundColor: '#000',
                opacity: 0.5
            },
            buttons: {
                OK: function() {
                    $(this).dialog('destroy');
                    $('#logoutButton').click();
                },
                Cancel: function() {
                    $(this).dialog('destroy');
                }
            }
        }
    );
    $('#confirm_logout_dialog').dialog('open');
}

function openConfirmDialog(title, delItemId) {
    $('#confirm_dialog').dialog({
            autoOpen: false,
            height: 200,
            width: 400,
            modal: true,
            title: title,
            overlay: {
                backgroundColor: '#000',
                opacity: 0.5
            },
            buttons: {
                OK: function() {
                    $(this).dialog('destroy');
                    $('#delItemId'+delItemId).click();
                    $('#itemId'+delItemId).remove();
                    updateTodoListTable();
                },
                Cancel: function() {
                    $(this).dialog('destroy');
                }
            }
        }
    );
    $('#confirm_dialog').dialog('open');
}

function openFormDialog(dialog, title, buttonId) {
    $(dialog).dialog({
            autoOpen: false,
            height: 400,
            width: 500,
            modal: true,
            title: title,
            overlay: {
                backgroundColor: '#000',
                opacity: 0.5
            },
            buttons: {
                OK: function() {
                    $(buttonId).click();
                    $(this).dialog('destroy');
                },
                Cancel: function() {
                    $(this).dialog('destroy');
                }
            }
        }
    );
    $(dialog).dialog('open');
}

