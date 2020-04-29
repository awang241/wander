<template>

    <div class="container">
        <h1 class="title is-5">Edit Email Addresses</h1>

        <b-field label="Current Primary Email Address:"></b-field>
        <h2>{{primaryEmail}}</h2>
        <br>

        <form>
            <div v-if="optionalEmails.length>0">
            <b-field label="Change your primary email">
                <b-select v-model="newPrimaryEmail" class="selectNewPEList" expanded>
                    <option class="singleEmail" v-for="email in optionalEmails" :key="email">{{email}}</option>
                </b-select>
            </b-field>
            <b-button  type="is-info" @click="changePrimaryEmail()">
                Change
            </b-button>
            </div>
            <br>

            <b-field label="Enter in an email address and click the + sign to add it to your profile! (5 email limit)" expanded></b-field>
            <b-field group-multiline grouped>
                <b-input type="email" class="addForm" v-model="newEmail" placeholder="Enter an email" maxlength="30" expanded ></b-input>
                <b-button class="addButton" type="is-info" @click="addEmail()">Add</b-button>
            </b-field>
        </form>

        <list v-bind:chosenItems="optionalEmails" v-on:deleteListItem="deleteEmail"></list>

        <b-field>
            <b-button type="is-info" @click="submitEmails()">Save</b-button>
        </b-field>

    </div>
</template>

<script>

    import List from "../List";

    export default {
        name: "EditEmails",
        components: {List},
        props: ["profile"],
        methods: {
            addEmail() {
                if (this.optionalEmails.length > 3) {
                    this.showWarning("Maximum email addresses reached")
                } else if (this.optionalEmails.includes(this.newEmail) || this.newEmail === this.primaryEmail) {
                    this.showWarning("Email address already in use")
                } else if (this.newEmail === "" || this.newEmail.trim().length === 0 || !this.newEmail.includes('@', 0)) {
                    this.showWarning("Please enter a valid email address")
                } else {
                    this.optionalEmails.push(this.newEmail)
                    this.newEmail = ""
                }
            },
            changePrimaryEmail() {
                if(this.newPrimaryEmail === "") {
                    this.$buefy.toast.open({
                        duration: 5000,
                        message: "No additional email address selected",
                        type: 'is-danger',
                        position: 'is-top',
                        queue: false,
                    });
                } else {
                    this.optionalEmails.push(this.primaryEmail);
                    this.optionalEmails = this.optionalEmails.filter(email => email != this.newPrimaryEmail)
                    this.primaryEmail = this.newPrimaryEmail;
                }
            },
            deleteEmail(emailToDelete){
                this.optionalEmails = this.optionalEmails.filter(email => email != emailToDelete)
            },
            submitEmails(){
                this.$buefy.toast.open({
                    duration: 5000,
                    message: "Changes Saved!",
                    type: 'is-success',
                    position: 'is-top',
                    queue: false,
                });
                this.$parent.updateEmails(this.primaryEmail, this.optionalEmails)
                }
            }  ,
            showWarning(message) {
                this.$buefy.toast.open({
                    duration: 5000,
                    message: message,
                    type: 'is-danger',
                    position: 'is-top',
                    queue: false,
                });
            },
            showSuccess(message){
                this.$buefy.toast.open({
                    duration:5500,
                    message: message,
                    type: 'success',
                    position: 'is-top'
                })
            },
        data() {
            return {
            primaryEmail: this.profile.primary_email,
            optionalEmails: this.profile.additional_email,
            newEmail: "",
            newPrimaryEmail: "",
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