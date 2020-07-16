<template>
        <div class="container">
                <h1 class="title">Create Account</h1>
                    <form @submit.prevent="createUser">

                        <ValidationProvider rules="required" name="Names" v-slot="{ errors, valid }">
                            <b-field group-multiline grouped>
                                <b-field label="First Name"
                                         :type="{ 'is-danger': errors[0], 'is-success': valid }"
                                         :message="errors"
                                         expanded >
                                    <b-input v-model="firstName" placeholder="First Name" required></b-input>
                                </b-field>
                                <b-field label="Last Name"
                                         :type="{ 'is-danger': errors[0], 'is-success': valid }"
                                         :message="errors"
                                         expanded>
                                    <b-input v-model="lastName" placeholder="Last Name" required></b-input>
                                </b-field>
                            </b-field>
                        </ValidationProvider>

                        <ValidationProvider rules="required|email" name="Email" v-slot="{ errors, valid }">
                            <b-field label="Email"
                                     :type="{ 'is-danger': errors[0], 'is-success': valid }"
                                     :message="errors"
                                     expanded>
                                     <b-input type="email"
                                         v-model="email"
                                         placeholder="Email"
                                         required>
                                     </b-input>
                            </b-field>
                        </ValidationProvider>

<!--                        <ValidationProvider rules="required|email" name="Email" v-slot="{ errors, valid }">-->
<!--                            <b-field-->
<!--                                    label="Email"-->
<!--                                    :type="{ 'is-danger': errors[0], 'is-success': valid }"-->
<!--                                    :message="errors"-->
<!--                            >-->
<!--                                <b-input type="email" v-model="email"></b-input>-->
<!--                            </b-field>-->
<!--                        </ValidationProvider>-->

                        <ValidationProvider rules="required" name="Password" v-slot="{ errors, valid }">
                            <b-field label="Password"
                                     :type="{ 'is-danger': errors[0], 'is-success': valid }"
                                     :message="errors"
                                     expanded>
                                <b-input v-model="password" type="password" placeholder="Password"
                                         required></b-input>
                            </b-field>
                        </ValidationProvider>

                        <b-field label="Confirm Password"
                                 :message="[{'Passwords do not match':isDisabled}]" expanded>
                            <b-input v-model="confpassword" type="password" placeholder="Confirm Password"
                                     required></b-input>
                        </b-field>


                        <b-field group-multiline grouped>
                            <b-field label="Date of Birth" expanded>
                                <b-datepicker
                                        editable
                                        :use-html5-validation="false"
                                        placeholder="Select Date of Birth"
                                        :date-formatter="dateFormatter"
                                        :min-date="minDate"
                                        :max-date="maxDate" ref="dateOfBirth"
                                        v-model="dateOfBirth"
                                        type="date" required
                                        validation-message="Please enter a valid date"

                                        pattern="^(?:(?:31(\/|-|\.)(?:0?[13578]|1[02]))\1|(?:(?:29|30)(\/|-|\.)(?:0?[13-9]|1[0-2])\2))(?:(?:1[6-9]|[2-9]\d)?\d{2})$|^(?:29(\/|-|\.)0?2\3(?:(?:(?:1[6-9]|[2-9]\d)?(?:0[48]|[2468][048]|[13579][26])|(?:(?:16|[2468][048]|[3579][26])00))))$|^(?:0?[1-9]|1\d|2[0-8])(\/|-|\.)(?:(?:0?[1-9])|(?:1[0-2]))\4(?:(?:1[6-9]|[2-9]\d)?\d{2})$">
                                    >
                                </b-datepicker>
                            </b-field>

                            <b-field label="Gender" expanded>
                                <b-select
                                        placeholder="Choose a gender"
                                        v-model="gender" required expanded>
                                    <option value="female">Female</option>
                                    <option value="male">Male</option>
                                    <option value="non-Binary">Non-Binary</option>
                                </b-select>
                            </b-field>
                        </b-field>

                            <b-field label="Fitness Level" expanded >
                                <b-select v-model="fitness" placeholder="Fitness Level" expanded>
                                    <option value="0">Beginner: I am not active at all </option>
                                    <option value="1">Novice: I do a low level of exercise (walking)</option>
                                    <option value="2">Intermediate: I work out 1-2 times per week</option>
                                    <option value="3">Advanced: I work out 3-4 times per week</option>
                                    <option value="4">Pro: I work out 5+ times per week</option>
                                </b-select>
                            </b-field>

                    <b-field>
                        <b-button native-type="submit" :disabled="isDisabled">Submit</b-button>
                    </b-field>
            </form>

            <validation-provider rules="required" v-slot="{ errors }">
                <input v-model="value" name="myinput" type="text" />
                <span>{{ errors[0] }}</span>
            </validation-provider>

            <ValidationProvider rules="required|email" name="Email" v-slot="{ errors, valid }">
                <b-field
                        label="Email"
                        :type="{ 'is-danger': errors[0], 'is-success': valid }"
                        :message="errors"
                >
                    <b-input type="email" v-model="email"></b-input>
                </b-field>
            </ValidationProvider>
        </div>

</template>




<script>
    import api from '../Api';
    import router from '../router.js'
    import toastMixin from '../mixins/toastMixin'

    import {ValidationProvider, extend} from 'vee-validate'
    import { required } from 'vee-validate/dist/rules';

    extend('required', {
        ...required,
        message: 'This field is required'
    });

    export default {
        name: "Registration",
        components: {
            ValidationProvider
        },
        mixins: [toastMixin],
        data() {
            const today = new Date()
            return {
                firstName: "",
                lastName: "",
                middleName: "",
                nickName: "",
                primary_email: "",
                password: "",
                confpassword: "",
                bio: "",
                dateOfBirth: "",
                gender: null,
                fitness: null,
                allUsers: null,
                date: new Date(),
                maxDate: new Date(today.getFullYear(), today.getMonth(), today.getDate()),
                minDate: new Date(today.getFullYear() -100, today.getMonth(), today.getDate())


            }
        },
        mounted() {
            let today = new Date().toISOString().split('T')[0];
            this.$refs.dateOfBirth.setAttribute('max', today);
        },

        computed: {
            isDisabled() {
                return (this.password !== this.confpassword);
            }
        },

        methods: {
            createUser() {
                if (this.validateEmail(this.primary_email)) {
                    return
                }
                if (this.password.length < 8) {
                    this.warningToast("Password must be 8 characters long")
                } else {
                    this.dateOfBirth.setHours(23)
                    api.createProfile({
                        lastname: this.lastName,
                        firstname: this.firstName,
                        middlename: this.middleName,
                        nickname: this.nickName,
                        primary_email: this.email,
                        additional_email: [],
                        password: this.password,
                        bio: this.bio,
                        date_of_birth: this.dateOfBirth,
                        gender: this.gender,
                        fitness: this.fitness,
                        passports: [],
                        activities: [],
                        authLevel: 5
                    })
                        .then(() => {
                            this.successToast("Account created!")
                            router.push('Login')
                        })
                        .catch(error => this.warningToast(error.response.data))
                }
            },

            validateEmail(email) {
                const re = /^(([^<>()[\]\\.,;:\s@"]+(\.[^<>()[\]\\.,;:\s@"]+)*)|(".+"))@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\])|(([a-zA-Z\-0-9]+\.)+[a-zA-Z]{2,}))$/;
                return re.test(String(email).toLowerCase());
            },

            dateFormatter(dt){
                return dt.toLocaleDateString('en-GB', { year: 'numeric', month: 'numeric', day: 'numeric' });
            }
        }
    }
</script>


<style scoped>
    .container {
        width: 800px;
    }

    @media only screen and (max-width: 600px) {
        .container {
            width: 100%;
        }
    }

</style>
