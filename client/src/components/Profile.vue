<template>
    <div class="container">
        <ul>
            <li v-for="(value, key) in currentUser" v-bind:key="key">
                {{key}} : {{value}}
            </li>
        </ul>

        <button class="btn-light" type="button" v-on:click="editDetails()">Edit Details</button>
    </div>
</template>

<script>
    import axios from 'axios'

    export default {
        name: "Profile",
        data() {
            return {
                currentUser: null,
                email: "bobby@google.com"
            }
        },
        mounted() {
            axios.get("https://f91246de-53d1-425e-9b1b-5524c2b62a0e.mock.pstmn.io/getusers")
                .then((response) => {
                    let rows =  response.data['users']
                    for(let i=0, len=rows.length; i<len; i++){
                        if(rows[i].email === this.email){
                            this.currentUser = rows[i]
                        }
                    }
                })
        },
    }
</script>

<style scoped>

    button:hover {
        opacity: 0.7;
    }

</style>