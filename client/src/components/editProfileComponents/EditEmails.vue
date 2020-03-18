<template>

    <div class="container">
        <h1 class="Title">Edit Email Adresses</h1>

        <h2 class="primaryEmailHeader">
            Current Primary Email Address
        </h2>

        <label class="primaryEmailText primaryEmailBox">
            {{primaryEmail}}
        </label>

        <b-button class="changeButton" type="is-info" @click="changePrimaryEmail()">
            Change
        </b-button>

        <form>
            <b-field label="Enter in an email address and click the + sign to add it to your profile! (5 email limit)" class="addEmails">
                <b-input type="email" class="addForm" v-model="newEmail" placeholder="n email's left" maxlength="30" ></b-input>
                <b-button class="addButton" type="is-info" @click="addEmail()">
                    +
                </b-button>
            </b-field>
        </form>

        <b-field label="Change your primary email" class="changePrimaryEmailMenuLoc">
            <b-select placeholder="Change" v-model="newPrimaryEmail">
                <option v-for="email in optionalEmails" :key="email">{{email}}</option>
            </b-select>
        </b-field>

        <list v-bind:chosenItems="optionalEmails" v-on:deleteListItem="deleteEmail"></list>
    </div>
</template>

<script>

    import List from "../List";
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
            showWarning(message) {
                this.$buefy.snackbar.open({
                    duration: 5000,
                    message: message,
                    type: 'is-danger',
                    position: 'is-bottom-left',
                    queue: false,
                })
            },
        },
        data() {
            return {
            primaryEmail: "bab@gmail.com",
            newEmail: "",
            newPrimaryEmail: "",
            optionalEmails: [],
            }
        }
    }
</script>

<style scoped>

    /*.container {*/
    /*    height:500px;*/
    /*}*/
    /*.Title {*/
    /*    font-size:35px;*/
    /*}*/
    /*.primaryEmailHeader {*/
    /*    font-size: 20px;*/
    /*}*/
    /*.primaryEmailText {*/
    /*    position:absolute;*/
    /*    top:140px;*/
    /*    font-size:15px;*/
    /*}*/
    /*.addEmails {*/
    /*    position:absolute;*/
    /*    top:180px;*/
    /*}*/
    /*.addForm {*/
    /*    width:160%;*/
    /*}*/
    /*.addButton {*/
    /*    position:absolute;*/
    /*    right:230px;*/
    /*    top:100px;*/
    /*}*/
    /*.primaryEmailBox{*/
    /*    border-radius: 0px;*/
    /*    padding:5px;*/
    /*    background:#f4f4f4;*/
    /*}*/
    /*.changePrimaryEmailMenuLoc {*/
    /*    position:absolute;*/
    /*    top:260px;*/
    /*}*/
    /*.changeButton {*/
    /*    position:absolute;*/
    /*    left:310px;*/
    /*    top:293px;*/
    /*}*/

</style>