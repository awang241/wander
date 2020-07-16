import { required, confirmed, email } from "vee-validate/dist/rules";
import { extend } from "vee-validate";

extend("required", {
    ...required,
    message: "This field is required boss"
});

extend("email", {
    ...email,
    message: "This field must be a valid email (contains an @)"
});



extend("confirmed", {
    ...confirmed,
    message: "This field confirmation does not match"
});
