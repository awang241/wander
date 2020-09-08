<template>
    <div class="container containerColor">
        <h1 class="title">Home</h1>
        <div id="results" class="column" v-if="notifications.length">
            <div v-for="notification in notifications"
                :key="notification.time">
            <Notification :notification="notification"/>
            <br>
            </div>
        </div>
        <div v-else id="noMatches">
            <h1>You have no notifications.</h1>
        </div>
        <observer v-on:intersect="$emit('loadMoreNotifications')"/>
    </div>
</template>

<script>
import store from "../store";
import toastMixin from "../mixins/toastMixin";
import Observer from "./Observer";
import api from "../Api";
import Notification from "../components/Notification";

export default {
  name: "HomeFeed",
  components: {
    Observer,
    Notification
  },
  mixins: [toastMixin],
  data() {
    return {
      store: store,
      observer: null,
      notifications: [],
    }
  },
  methods: {
      loadMoreNotifications(){
          api.getNotifications(this.store.getters.getUserId, localStorage.getItem("authToken")).then(response => {
              if(response.data.length > 0){
                  this.notifications = response.data;
            }
          })
      }
  },
  mounted() {

      //TODO Testing data here. Replace with real data when backend is sorted out
      for (let i = 1; i < 5; i++) {
          let notification = {
              message: `This is my own dummy notification #${i}`,
              notificationType: i,
              dateTime: "2020-09-05T08:00:00+1300",
              activityId: 45011,
              profileId: null
          };
          this.notifications.push(notification);
      }
      for (let i = 0; i < 11; i++) {
          let notification = {
              message: `This is dummy notification #${i}`,
              notificationType: i,
              dateTime: "2020-09-05T08:00:00+1300",
              activityId: 45011,
              profileId: 0
          };
          this.notifications.push(notification);
      }

    //End of test data
  }
}
</script>

<style scoped>

</style>