import { required, confirmed, email } from "vee-validate/dist/rules";
import { extend } from "vee-validate";

extend('minName', value => {

    if (value.length >= 2) {
        return true;
    }

    return '{_field_} must be at least 2 characters long';
});


extend('minPassword', value => {

    if (value.length >= 8) {
        return true;
    }

    return 'Password must be at least 8 characters long';
});

extend("requiredGender", {
    ...required,
    message: "You must choose a gender"
});

extend("required", {
    ...required,
    message: "{_field_} is required"
});

extend("requiredConfirm", {
    ...required,
    message: "You must re-enter your password for validation purposes"
});

extend("email", {
    ...email,
    message: "This field must be a valid email (contains an @)"
});

extend("confirmed", {
    ...confirmed,
    message: "These passwords do not match"
});

extend("maxBirthDate", value => {
    const today = new Date()
    let maxDate = new Date(today.getFullYear(), today.getMonth(), today.getDate())

    if (value.getTime() < maxDate.getTime()) {
        return true
    }

    return "Cannot choose a future date"

});
