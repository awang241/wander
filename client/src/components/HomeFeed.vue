<template>
    <div class="container containerColor">
        <h1 class="title">Home</h1>
        <div id="results" class="column" v-if="notifications.length">
            <div v-for="notification in notifications"
                :key="notification.timeStamp">
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
        const searchParameters = {count: 50, startIndex: 0}
        api.getNotifications(Number(this.store.getters.getUserId), localStorage.getItem("authToken"), searchParameters).then(response => {
            if(response.data.notifications.length > 0){
              this.notifications = response.data.notifications;
            }
          })
      }
  },
  mounted() {
    this.loadMoreNotifications()
  }
}
</script>

<style scoped>

</style>