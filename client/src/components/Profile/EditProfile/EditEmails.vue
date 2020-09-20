<template>

    <div class="container">
        <h1 class="title is-5">Edit Email Addresses</h1>

        <b-field label="Current Primary Email Address:"></b-field>
        <h2>{{primaryEmail}}</h2>
        <br>

        <form>
            <div v-if="optionalEmails.length>0">

                <ValidationProvider rules="changeEmail" name="Primary Email" v-slot="{ errors, valid }" slim>
                    <b-field label="Change your primary email"
                             :type="{'is-danger': errors[0], 'is-success': valid}"
                             :message="errors">
                        <b-select v-model="newPrimaryEmail" class="selectNewPEList" expanded>
                            <option class="singleEmail" v-for="email in optionalEmails" :key="email">{{email}}</option>
                        </b-select>
                    </b-field>
                </ValidationProvider>

                <b-button type="is-info" @click="changePrimaryEmail()">
                    Change
                </b-button>
            </div>
            <br>

            <ValidationProvider rules="optionalEmail|email" name="Email" v-slot="{ errors, valid }" slim>
                <b-field label="Add optional email addresses"
                         :type="{'is-danger': errors[0], 'is-success': valid}"
                         :message="errors"
                         expanded>
                    <b-input type="email" class="addForm" v-model="newEmail" placeholder="Enter an email" maxlength="30" expanded></b-input>
                </b-field>
            </ValidationProvider>
            <b-button class="addButton" type="is-primary" @click="addEmail()">Add</b-button>
        </form>

        <list v-bind:chosenItems="optionalEmails" v-on:deleteListItem="deleteEmail"></list>

        <b-field>
            <b-button style="float:right" type="is-primary" @click="submitEmails()">Save</b-button>
        </b-field>
        <br>
    </div>
</template>

<script>

    import List from "../../Misc/HelperComponents/List";
    import toastMixin from "../../../mixins/toastMixin";
    import {ValidationProvider} from "vee-validate";


    export default {
        name: "EditEmails",
        mixins: [toastMixin],
        props: ["profile"],
        components: {
            ValidationProvider,
            List
        },
        data() {
            return {
                primaryEmail: this.profile.primary_email,
                optionalEmails: this.profile.additional_email,
                newEmail: "",
                newPrimaryEmail: "",
                originalOptionalEmails: JSON.parse(JSON.stringify(this.profile.additional_email)),
                originalPrimaryEmail: JSON.parse(JSON.stringify(this.profile.primary_email))
            }
        },
        methods: {
            addEmail() {
                if (this.optionalEmails.length > 3) {
                    this.warningToast("Maximum email addresses reached")
                } else if (this.optionalEmails.includes(this.newEmail) || this.newEmail === this.primaryEmail) {
                    this.warningToast("Email address already in use")
                } else if (this.newEmail === "" || this.newEmail.trim().length === 0 || !this.newEmail.includes('@', 0)) {
                    this.warningToast("Please enter a valid email address")
                } else {
                    this.optionalEmails.push(this.newEmail);
                    this.newEmail = "";
                }
            },
            changePrimaryEmail() {
                if (this.newPrimaryEmail === "") {
                    this.warningToast("No additional email address selected")
                } else {
                    this.optionalEmails.push(this.primaryEmail);
                    let index = this.optionalEmails.indexOf(this.newPrimaryEmail);
                    this.optionalEmails.splice(index, 1);
                    this.primaryEmail = this.newPrimaryEmail;
                }
            },
            deleteEmail(emailToDelete) {
                this.optionalEmails = this.optionalEmails.filter(email => email != emailToDelete)
            },
            submitEmails(){
                if ((this.primaryEmail === this.originalPrimaryEmail) && (this.originalOptionalEmails.toString() === this.optionalEmails.toString())) {
                    this.warningToast("No changes made")
                } else {
                    this.$parent.updateEmails(this.primaryEmail, this.optionalEmails)
                    this.successToast("New emails saved")
                }
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

</style>