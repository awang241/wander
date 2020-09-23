import { alpha_spaces, required, confirmed, email, integer } from "vee-validate/dist/rules";
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

extend('changeEmail', {
    ...required,
    message: 'Must choose an additional email'
})

extend('alphabeticCharsOrSpaces',  {
    ...alpha_spaces,
    message: '{_field_} can only contain alphabetic characters and spaces'
})


extend('optionalEmail', {
    ...required,
    message:" Add an optional email if you'd like"
});

extend('activityDescription', value => {
   if (value.split("").length < 3) {
       return "Description must contain at least 2 words"
   }
   return true
});

extend("requiredGender", {
    ...required,
    message: "You must choose a gender"
});

extend("requiredActivityType", {
    ...required,
    message: "You must choose at least one activity type"
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
    message: "This field must be a valid email (contains an @, a fullstop, then some more characters)"
});



extend("confirmed", {
    ...confirmed,
    message: "These passwords do not match"
});

extend("maxDistanceInRange", value => {
    if (!isNaN(value) && value >= 1 && value <= 200) {
        return true;
    }
    return "{_field_} must be an integer between 1 and 200";
});

extend("integer", {
    ...integer,
    message: "The value entered is not an integer"
});

extend("maxBirthDate", value => {
    const today = new Date()
    let maxDate = new Date(today.getFullYear(), today.getMonth(), today.getDate())

    if (value.getTime() < maxDate.getTime()) {
        return true
    }

    return "Cannot choose a future date"

});
