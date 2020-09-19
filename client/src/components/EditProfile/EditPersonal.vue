<template>

    <div class="container">

        <h1 class="title is-5">Edit Basic Info</h1>
        <ValidationObserver v-slot="{ handleSubmit }">

            <form @submit.prevent="handleSubmit(sendUpdatedData)">
                <b-field group-multiline grouped>
                    <ValidationProvider rules="required|minName|alphabeticCharsOrSpaces" name="First Name" v-slot="{ errors, valid }" slim>
                        <b-field label="First Name"
                                 :type="{ 'is-danger': errors[0], 'is-success': valid }"
                                 :message="errors"
                                 expanded>
                            <template slot="label">First Name <span>*</span></template>
                            <b-input v-model="firstName" placeholder="First Name"></b-input>
                        </b-field>
                    </ValidationProvider>
                    <ValidationProvider name="Middle Name" v-slot="{ errors, valid }" slim>
                        <b-field label="Middle Name"
                                 :type="{ 'is-danger': errors[0], 'is-success': valid }"
                                 :message="errors"
                                 expanded>
                            <template slot="label">Middle Name</template>
                            <b-input v-model="middleName" placeholder="Middle Name"></b-input>
                        </b-field>
                    </ValidationProvider>
                    <ValidationProvider rules="required|minName|alphabeticCharsOrSpaces" name="Last Name" v-slot="{ errors, valid }" slim>
                        <b-field label="Last Name"
                                 :type="{ 'is-danger': errors[0], 'is-success': valid }"
                                 :message="errors"
                                 expanded>
                            <template slot="label">Last Name <span>*</span></template>
                            <b-input v-model="lastName" placeholder="Last Name"></b-input>
                        </b-field>
                    </ValidationProvider>
                </b-field>
                <ValidationProvider name="Nickname" v-slot="{ errors, valid }" slim>
                    <b-field label="Nickname"
                             :type="{ 'is-danger': errors[0], 'is-success': valid }"
                             :message="errors"
                             expanded>
                        <template slot="label">Nickname</template>

                        <b-input v-model="nickName" type="text" placeholder="Nickname"></b-input>
                    </b-field>
                </ValidationProvider>


                <b-field group-multiline grouped>
                    <ValidationProvider rules="required" name="Date of Birth" v-slot="{ errors, valid }">
                        <b-field label="Date of birth"
                                 :type="{ 'is-danger': errors[0], 'is-success': valid }"
                                 :message="errors" expanded>
                            <template slot="label">Date of Birth <span>*</span></template>
                            <input v-model="dateOfBirth" class="input" type="date"
                                   :max="maxDate.toISOString().split('T')[0]"
                                   :min="minDate.toISOString().split('T')[0]">
                        </b-field>
                    </ValidationProvider>

                    <ValidationProvider rules="requiredGender" name="Gender" v-slot="{ errors, valid }" slim>
                        <b-field label="Gender"
                                 :type="{ 'is-danger': errors[0], 'is-success': valid }"
                                 :message="errors"
                                 expanded>
                            <template slot="label">Gender <span>*</span></template>

                            <b-select
                                    placeholder="Choose a gender"
                                    v-model="gender"
                                    expanded>
                                <option value="female">Female</option>
                                <option value="male">Male</option>
                                <option value="non-Binary">Non-Binary</option>
                            </b-select>
                        </b-field>
                    </ValidationProvider>

                </b-field>

                <ValidationProvider rules="required" name="FitnessLevel" v-slot="{ errors, valid }" slim>
                    <b-field label="Fitness Level"
                             :type="{ 'is-danger': errors[0], 'is-success': valid }"
                             :message="errors"
                             expanded>
                        <template slot="label">Fitness Level <span>*</span></template>

                        <b-select v-model="fitness_level" placeholder="Fitness Level" expanded>
                            <option value="0">Beginner: I am not active at all</option>
                            <option value="1">Novice: I do a low level of exercise (walking)</option>
                            <option value="2">Intermediate: I work out 1-2 times per week</option>
                            <option value="3">Advanced: I work out 3-4 times per week</option>
                            <option value="4">Pro: I work out 5+ times per week</option>
                        </b-select>
                    </b-field>
                </ValidationProvider>

                <ValidationProvider name="Bio" v-slot="{ errors, valid }" slim>

                    <b-field label="Bio"
                             :type="{ 'is-danger': errors[0], 'is-success': valid }"
                             :message="errors"
                             expanded>
                        <template slot="label">Bio</template>
                        <b-input v-model="bio" maxlength="200" type="textarea" placeholder="Enter a bio"></b-input>
                    </b-field>
                </ValidationProvider>
                <b-field>
                    <b-button style="float:right" type="is-primary" native-type="submit">Save</b-button>
                </b-field>
                <br>

            </form>
        </ValidationObserver>
    </div>
</template>

<script>
    import {ValidationProvider, ValidationObserver} from 'vee-validate'
    import toastMixin from "../../mixins/toastMixin";

    export default {
        name: "EditPersonal",
        props: ["profile"],
        components: {
            ValidationProvider,
            ValidationObserver
        },
        mixins: [toastMixin],

        data() {
            const today = new Date();
            return {
                firstName: this.profile.firstname,
                lastName: this.profile.lastname,
                middleName: this.profile.middlename,
                nickName: this.profile.nickname,
                bio: this.profile.bio,
                dateOfBirth: this.profile.date_of_birth,
                gender: this.profile.gender,
                fitness_level: this.profile.fitness,
                date: this.profile.date_of_birth,
                maxDate: new Date(today.getFullYear(), today.getMonth(), today.getDate()),
                minDate: new Date(today.getFullYear() - 120, today.getMonth(), today.getDate())
            }
        },
        mounted() {
            switch (this.fitness_level) {
                case 0 :
                    this.fitness_statement = "Beginner: I am not active at all";
                    break;
                case 1 :
                    this.fitness_statement = "Novice: I do a low level of exercise (walking)";
                    break;
                case 2 :
                    this.fitness_statement = "Intermediate: I work out 1-2 times per week";
                    break;
                case 3 :
                    this.fitness_statement = "Advanced: I work out 3-4 times per week";
                    break;
                case 4 :
                    this.fitness_statement = "Pro: I work out 5+ times per week";
                    break;
                default:
                    this.fitness_statement = "Beginner: I am not active at all";
            }
        },

        methods: {
            sendUpdatedData() {
                const original =
                    {
                        "firstname": this.profile.firstname,
                        "lastname": this.profile.lastname,
                        "middlename": this.profile.middlename,
                        "nickname": this.profile.nickname,
                        "bio": this.profile.bio,
                        "date_of_birth": new Date(this.profile.date_of_birth),
                        "gender": this.profile.gender,
                        "fitness": this.profile.fitness
                    };

                const personalDetails =
                    {
                        "firstname": this.firstName,
                        "lastname": this.lastName,
                        "middlename": this.middleName,
                        "nickname": this.nickName,
                        "bio": this.bio,
                        "date_of_birth": Date.parse(this.dateOfBirth),
                        "gender": this.gender,
                        "fitness": this.fitness_level
                    };
                if (JSON.stringify(original) !== JSON.stringify(personalDetails)) {
                    this.$parent.updatePersonal(personalDetails);
                    this.successToast("New personal details saved")
                } else {
                    this.warningToast("No changes made")
                }
            },
            loadProfileData() {
                this.firstName = this.profile.firstname;
                this.lastName = this.profile.lastname;
                this.middleName =  this.profile.middlename;
                this.nickName =  this.profile.nickname;
                this.bio = this.profile.bio;
                this.dateOfBirth = this.profile.date_of_birth;
                this.gender = this.profile.gender;
                this.fitness_level = this.profile.fitness;
                this.date = this.profile.date_of_birth;
            },
            getErrorMessageFromStatusCode(statusCode) {
                let message = "";
                if (statusCode === 200) {
                    message = "Details updated successfully"
                } else if (statusCode === 400 || statusCode === 403 || statusCode === 401) {
                    message = "Please fill in all required fields"
                }
                this.warningToast(message)
            },
            dateFormatter(dt) {
                return dt.toLocaleDateString('en-NZ', {year: 'numeric', month: 'numeric', day: 'numeric'});
            }
        },
        watch: {
            profile: function() {
                this.loadProfileData();
            }
        }
    }
</script>

<style scoped>
    .container {
        background-color: #F7F8F9;
        margin-top: 0px;
        padding: 0px;
    }

    span {
        color: red;
    }


</style>