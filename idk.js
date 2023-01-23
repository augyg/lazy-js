
##################### FIRST SET EVENT LISTENER ON DOM LOAD ################################

// <script type="text/javascript">
// var acicActionType = false;
// P.when('auth-validate-form-handler', "acic-component", "ready").execute('acic-setup', function(AuthValidateFormHandler) {
//     var aautTargetForm = document.querySelector('form[name="signIn"]');
//     if (aautTargetForm) {
//         setAAToken("136-5540821-8712564-" + Date.now(), aautTargetForm);
//         if ((typeof acic !== 'undefined') && (acic != null)) {
//             acic.setupACIC({
//                 "data-mode": "1", 
//                 "data-ref-id": "ap",
//                 "data-context": getClientContext(aautTargetForm),
//                 "data-callback": function(data) {
//                     if (data.sessionToken) {
//                         setAAToken(data.sessionToken, aautTargetForm);
//                     }
//                     if (data.actionType && data.actionType !== "PASS") {
//                         acicActionType = true;
//                     }
//                 },
//                 "data-host-config": "prod.USAmazon"
//             });
//         }

        var submitInputs = aautTargetForm.querySelectorAll('input[type="submit"]');
        if (submitInputs && submitInputs.length === 1) {
            var submitInput = submitInputs[0];
            submitInput.addEventListener("click", function(event) {
                acicEventListener(AuthValidateFormHandler, aautTargetForm, submitInput, event);
            });
        }
    }
});
