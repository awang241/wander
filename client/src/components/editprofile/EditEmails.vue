<template>

    <div class="container">
        <h1 class="Title">Edit Email Addresses</h1>

        <b-field label="Current Primary Email Address:"></b-field>
        <h2>{{primaryEmail}}</h2>
        <br>

        <form>
            <div>
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
                <b-input type="email" class="addForm" v-model="newEmail" placeholder="n emails left" maxlength="30" expanded ></b-input>
                <b-button class="addButton" type="is-info" @click="addEmail()">Add</b-button>
            </b-field>
        </form>

        <list v-bind:chosenItems="optionalEmails" v-on:deleteListItem="deleteEmail"></list>

        <b-field>
            <b-button type="is-info" @click="submitEmails">Save</b-button>
        </b-field>

    </div>
</template>

<script>

    import List from "../List";
    import authenticationStore from "../../store/authenticationStore";
    import profileStore from "../../store/profileStore";
    import Api from "../../Api";

    export default {
        name: "EditEmails",
        components: {List},
        methods: {
            addEmail() {
                if(this.optionalEmails.length > 3){
                    //Todo change this to inform the user
                  this.showWarning("Maximum emails reached")
                } else if(this.optionalEmails.includes(this.newEmail) || this.newEmail === this.primaryEmail){
                    this.showWarning("Email is already in use")
                } else {
                    this.optionalEmails.push(this.newEmail);
                }
            },
            changePrimaryEmail() {
                if(this.newPrimaryEmail === "") {
                    this.showWarning("No email selected")
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
                profileStore.methods.setOptionalEmails(this.optionalEmails)
                profileStore.methods.setPrimaryEmail(this.primaryEmail)
                Api.editEmail({
                    "primary_email": profileStore.data.primaryEmail,
                    "additional_email": profileStore.data.optionalEmails
                }, authenticationStore.methods.getUserId(), authenticationStore.methods.getSessionId())
                this.showSuccess("Emails submitted")
                }
            }  ,
            showWarning(message) {
                this.$buefy.snackbar.open({
                    duration: 5000,
                    message: message,
                    type: 'is-danger',
                    position: 'is-bottom-left',
                    queue: false,
                })
            },
            showSuccess(message){
                this.$buefy.toast.open({
                    duration:5500,
                    message: message,
                    type: 'success',
                    position: 'is-bottom'
                })
            },
        data() {
            return {
            primaryEmail: profileStore.data.primaryEmail,
            newEmail: "",
            newPrimaryEmail: "",
            optionalEmails: profileStore.data.optionalEmails,
            }
        }
    }
</script>

<style scoped>
    .container {
        background-color: #F7F8F9;
    }

</style>