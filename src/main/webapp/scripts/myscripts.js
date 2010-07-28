function updateTodoListTable() {
    $('#todolist_table').trigger("update");
}

function openConfirmLogoutDialog(message, title) {
    $('#confirm_logout_dialog').dialog({
            autoOpen: false,
            height: 200,
            width: 300,
            modal: true,
            title: title,
            open:function(){
                $('#confirm_logout_dialog').text(message);
            },
            overlay: {
                backgroundColor: '#000',
                opacity: 0.5
            },
            buttons: {
                OK: function() {
                    $(this).dialog('close');
                    $('#logoutButton').click();
                },
                Cancel: function() {
                    $(this).dialog('close');
                }
            }
        }
    );
    $('#confirm_logout_dialog').dialog('open');
}

function openConfirmDialog(message, title, delItemId) {
    $('#confirm_dialog').dialog({
            autoOpen: false,
            height: 200,
            width: 300,
            modal: true,
            title: title,
            open:function(){
                $('#confirm_dialog').text(message);
            },
            overlay: {
                backgroundColor: '#000',
                opacity: 0.5
            },
            buttons: {
                OK: function() {
                    $(this).dialog('close');
                    $('#delItemId'+delItemId).click();
                    $('#itemId'+delItemId).remove();
                    updateTodoListTable();
                },
                Cancel: function() {
                    $(this).dialog('close');
                }
            }
        }
    );
    $('#confirm_dialog').dialog('open');
}

function openUpdateTodoItemDialog() {
    $('#update_todoitem_dialog').dialog({
            autoOpen: false,
            height: 400,
            width: 500,
            modal: true,
            title: "作業更新",
            overlay: {
                backgroundColor: '#000',
                opacity: 0.5
            },
            buttons: {
                OK: function() {
                    $('#update_todoitem_button').click();
                    $(this).dialog('close');
                },
                Cancel: function() {
                    $(this).dialog('close');
                }
            }
        }
    );
    $('#update_todoitem_dialog').dialog('open');
}

