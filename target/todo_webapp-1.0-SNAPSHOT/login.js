function fieldChanged(){var C=getField("user_id");
var B=getField("password");
var D=true;
if(C.value.length>0&&B.value.length>0){D=false
}var A=getField("login");
if(D){A.setAttribute("disabled","true")
}else{A.removeAttribute("disabled")
}}function getField(B){var A=document.getElementById(B);
if(A==undefined){throw new Error("要�?��見つかりません: "+B)
}return A
};